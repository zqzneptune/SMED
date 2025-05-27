#' Evaluate Predicted Complexes Against Gold Standard Complexes
#'
#' Computes several metrics (Sn, PPV, Accuracy, Jaccard, PR score) to
#' evaluate a list of predicted protein complexes against a gold standard set.
#' This function is based on the evaluation metrics described by
#' Zhang X-F, et al. (2012) PLoS ONE 7(8): e43092.
#'
#' @param gold_standard_complexes A list of character vectors, where each
#'   vector represents a gold standard protein complex.
#' @param predicted_complexes A list of character vectors, where each
#'   vector represents a predicted protein complex.
#' @param num_cores Integer, the number of cores to use for parallel
#'   computation. Defaults to `getOption("mc.cores", 2L)`. For
#'   Windows, parallel processing is not supported by `mclapply`;
#'   consider using `BiocParallel` for cross-platform parallelization if needed.
#'
#' @return A named numeric vector containing:
#'   \item{ACC}{Accuracy, calculated as the geometric mean of Sn and PPV.}
#'   \item{Jaccd}{Overall Jaccard coefficient, combining weighted Jaccard
#'     scores for gold standard and predicted complexes.}
#'   \item{PR}{Overall PR score, combining weighted PR-like scores for
#'     gold standard and predicted complexes.}
#'   Returns `NULL` if no overlap is detected between the protein sets of
#'   gold standard and predicted complexes.
#' @export
#' @importFrom parallel mclapply detectCores
#' @references
#' Zhang X-F, Dai D-Q, Ou-Yang L, Wu M-Y (2012) Exploring Overlapping
#' Functional Units with Various Structure in Protein Interaction Networks.
#' PLoS ONE 7(8): e43092. \doi{10.1371/journal.pone.0043092}
#'
#' @examples
#' gs_cpx <- list(c("A", "B", "C"), c("C", "D", "E"))
#' pred_cpx <- list(c("A", "B"), c("C", "D"), c("F", "G"))
#' if (requireNamespace("parallel", quietly = TRUE)) {
#'   # Use a low number of cores for example, adjust as needed
#'   # On Windows, mclapply will run sequentially if num_cores > 1
#'   is_windows <- .Platform$OS.type == "windows"
#'   core_count <- if (is_windows) 1L else getOption("mc.cores", 2L)
#'   metrics <- evaluate_complexes(gs_cpx, pred_cpx, num_cores = core_count)
#'   print(metrics)
#' }
evaluate_complexes <- function(gold_standard_complexes,
                               predicted_complexes,
                               num_cores = getOption("mc.cores", 2L)) {

  if (!is.list(gold_standard_complexes) ||
      !is.list(predicted_complexes)) {
    stop("Inputs must be lists of complexes.")
  }
  if (num_cores < 1) num_cores <- 1L # Ensure at least one core

  # Ensure elements are character vectors
  gold_standard_complexes <- lapply(gold_standard_complexes, as.character)
  predicted_complexes <- lapply(predicted_complexes, as.character)

  sizes_gs <- lengths(gold_standard_complexes)
  sizes_pred <- lengths(predicted_complexes)

  # Overlap of predicted complexes with the combined set of proteins in GS
  proteins_in_gs <- unique(unlist(gold_standard_complexes))
  overlap_pred_vs_gs_proteins <- vapply(predicted_complexes, function(p_cpx) {
    length(intersect(p_cpx, proteins_in_gs))
  }, integer(1))

  if (sum(overlap_pred_vs_gs_proteins) == 0) {
    message("No overlap detected between predicted complex proteins and ",
            "gold standard complex proteins.")
    return(c("ACC" = 0, "Jaccd" = 0, "PR" = 0))
  }

  # Calculate intersection matrix (GS rows, Pred cols)
  # prLst: list where each element corresponds to a GS complex.
  # Each element is a vector of intersection sizes with all Pred complexes.
  intersection_list <- parallel::mclapply(gold_standard_complexes,
                                          function(gs_cpx) {
                                            vapply(predicted_complexes, function(p_cpx) {
                                              length(intersect(gs_cpx, p_cpx))
                                            }, integer(1))
                                          }, mc.cores = num_cores)
  intersection_matrix <- matrix(
    unlist(intersection_list),
    byrow = TRUE,
    ncol = length(predicted_complexes),
    nrow = length(gold_standard_complexes)
  )
  rownames(intersection_matrix) <- names(gold_standard_complexes)
  colnames(intersection_matrix) <- names(predicted_complexes)

  # Sensitivity (Sn)
  max_inter_gs <- apply(intersection_matrix, 1, max, na.rm = TRUE)
  # Denominator: sum of sizes of gold standard complexes
  # Ensure sizes_gs has non-zero sum to avoid division by zero
  sum_sizes_gs <- sum(sizes_gs)
  sensitivity <- if (sum_sizes_gs > 0) {
    sum(max_inter_gs) / sum_sizes_gs
  } else {
    0
  }


  # Positive Predictive Value (PPV)
  max_inter_pred <- apply(intersection_matrix, 2, max, na.rm = TRUE)
  # Denominator: sum of sizes of predicted complexes that overlap with GS proteins
  sum_overlap_pred <- sum(overlap_pred_vs_gs_proteins)
  ppv <- if (sum_overlap_pred > 0) {
    sum(max_inter_pred) / sum_overlap_pred
  } else {
    0
  }

  # Accuracy (ACC)
  accuracy <- sqrt(sensitivity * ppv)

  # Jaccard Index based measures
  jaccard_list <- parallel::mclapply(gold_standard_complexes,
                                     function(gs_cpx) {
                                       vapply(predicted_complexes, function(p_cpx) {
                                         intersect_len <- length(intersect(gs_cpx, p_cpx))
                                         union_len <- length(union(gs_cpx, p_cpx))
                                         if (union_len == 0) return(0) # Or 1 if both empty, though unlikely
                                         intersect_len / union_len
                                       }, numeric(1))
                                     }, mc.cores = num_cores)

  jaccard_matrix <- matrix(
    unlist(jaccard_list),
    byrow = TRUE,
    ncol = length(predicted_complexes),
    nrow = length(gold_standard_complexes)
  )

  max_jaccard_gs <- apply(jaccard_matrix, 1, max, na.rm = TRUE)
  weighted_jaccard_gs <- if (sum_sizes_gs > 0) {
    sum(max_jaccard_gs * sizes_gs) / sum_sizes_gs
  } else {
    0
  }

  max_jaccard_pred <- apply(jaccard_matrix, 2, max, na.rm = TRUE)
  # Ensure sum_sizes_pred is not zero
  sum_sizes_pred <- sum(sizes_pred)
  weighted_jaccard_pred <- if (sum_sizes_pred > 0) {
    sum(max_jaccard_pred * sizes_pred) / sum_sizes_pred
  } else {
    0
  }

  jaccard_overall <- if (weighted_jaccard_pred + weighted_jaccard_gs > 0) {
    (2 * weighted_jaccard_pred * weighted_jaccard_gs) /
      (weighted_jaccard_pred + weighted_jaccard_gs)
  } else {
    0
  }


  # PR Score (as defined in the paper)
  pr_score_list <- parallel::mclapply(gold_standard_complexes,
                                      function(gs_cpx) {
                                        len_gs <- length(gs_cpx)
                                        if (len_gs == 0) return(rep(0, length(predicted_complexes)))
                                        vapply(predicted_complexes, function(p_cpx) {
                                          len_p <- length(p_cpx)
                                          if (len_p == 0) return(0)
                                          intersect_len <- length(intersect(gs_cpx, p_cpx))
                                          (intersect_len^2) / (len_p * len_gs)
                                        }, numeric(1))
                                      }, mc.cores = num_cores)

  pr_score_matrix <- matrix(
    unlist(pr_score_list),
    byrow = TRUE,
    ncol = length(predicted_complexes),
    nrow = length(gold_standard_complexes)
  )

  max_pr_gs <- apply(pr_score_matrix, 1, max, na.rm = TRUE)
  weighted_pr_gs <- if (sum_sizes_gs > 0) {
    sum(max_pr_gs * sizes_gs) / sum_sizes_gs
  } else {
    0
  }


  max_pr_pred <- apply(pr_score_matrix, 2, max, na.rm = TRUE)
  weighted_pr_pred <- if (sum_sizes_pred > 0) {
    sum(max_pr_pred * sizes_pred) / sum_sizes_pred
  } else {
    0
  }

  pr_overall <- if (weighted_pr_pred + weighted_pr_gs > 0) {
    (2 * weighted_pr_gs * weighted_pr_pred) /
      (weighted_pr_gs + weighted_pr_pred)
  } else {
    0
  }


  return(
    c(
      "ACC" = accuracy,
      "Jaccd" = jaccard_overall,
      "PR" = pr_overall
    )
  )
}
