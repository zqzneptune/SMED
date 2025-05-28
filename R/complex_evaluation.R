#' Evaluate Predicted Complexes Against Gold Standard Complexes
#'
#' Computes several metrics (Sn, PPV, Accuracy, Jaccard, PR score) to
#' evaluate a list of predicted protein complexes against a gold standard set.
#' This function implements the evaluation metrics described by
#' Zhang X-F, et al. (2012) PLoS ONE 7(8): e43092.
#'
#' @section Evaluation Metrics:
#' \describe{
#'   \item{Sensitivity (Sn)}{Measures the fraction of proteins in gold standard 
#'     complexes that are correctly predicted. Weighted by complex size.}
#'   \item{Positive Predictive Value (PPV)}{Measures the fraction of predicted 
#'     complex proteins that match gold standard complexes. Weighted by overlap.}
#'   \item{Accuracy (ACC)}{Geometric mean of Sn and PPV, providing balanced 
#'     measure of both metrics.}
#'   \item{Jaccard Index}{Measures similarity between complexes based on shared 
#'     proteins relative to total proteins in either complex.}
#'   \item{PR Score}{Precision-Recall like score that balances coverage and 
#'     accuracy of predictions.}
#' }
#'
#' @section Statistical Methods:
#' The function uses weighted averages where larger complexes contribute more to 
#' the final score. All metrics are calculated in parallel when possible using 
#' `mclapply`. The weighting scheme ensures that:
#' \itemize{
#'   \item Large complexes don't dominate purely by size
#'   \item Small complexes still contribute meaningfully
#'   \item The final scores reflect overall prediction quality
#' }
#'
#' @section Biological Significance:
#' These metrics help assess how well predicted protein complexes match known 
#' biological complexes in terms of:
#' \itemize{
#'   \item Protein composition accuracy
#'   \item Functional unit identification
#'   \item Complex boundary determination
#'   \item Overall biological relevance
#' }
#'
#' @param gold_standard_complexes A list of character vectors, where each
#'   vector represents a gold standard protein complex (e.g., from CORUM).
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
#'
#' @section Benchmarking:
#' When benchmarking different prediction methods:
#' \itemize{
#'   \item Use the same gold standard for fair comparison
#'   \item Consider running multiple trials with different random seeds
#'   \item Report all three metrics (ACC, Jaccd, PR) for comprehensive evaluation
#' }
#'
#' @export
#' @importFrom parallel mclapply detectCores
#' @references
#' Zhang X-F, Dai D-Q, Ou-Yang L, Wu M-Y (2012) Exploring Overlapping
#' Functional Units with Various Structure in Protein Interaction Networks.
#' PLoS ONE 7(8): e43092. \doi{10.1371/journal.pone.0043092}
#'
#' @examples
#' # Basic example with small complexes
#' gs_cpx <- list(c("A", "B", "C"), c("C", "D", "E"))
#' pred_cpx <- list(c("A", "B"), c("C", "D"), c("F", "G"))
#' metrics <- evaluate_complexes(gs_cpx, pred_cpx)
#' print(metrics)
#'
#' # Real-world example using included data
#' if (requireNamespace("parallel", quietly = TRUE) {
#'   data(RefCORUM_Havugimana_PC_Cell_2012_n_324, package = "SMED")
#'   data(PredCpx_Havugimana_PC_Cell_2012_n_622, package = "SMED")
#'   
#'   # Subset to first 50 complexes for example
#'   gs_sub <- RefCORUM_Havugimana_PC_Cell_2012_n_324[1:50]
#'   pred_sub <- PredCpx_Havugimana_PC_Cell_2012_n_622[1:50]
#'   
#'   # Evaluate with 2 cores (if available)
#'   metrics <- evaluate_complexes(gs_sub, pred_sub, num_cores = 2)
#'   print(metrics)
#' }
evaluate_complexes <- function(gold_standard_complexes,
                             predicted_complexes,
                             num_cores = getOption("mc.cores", 2L)) {
  
  # Input validation
  if (!is.list(gold_standard_complexes) || !is.list(predicted_complexes)) {
    stop("Inputs must be lists of complexes.")
  }
  if (num_cores < 1) num_cores <- 1L
  
  # Prepare inputs
  gold_standard_complexes <- lapply(gold_standard_complexes, as.character)
  predicted_complexes <- lapply(predicted_complexes, as.character)
  sizes_gs <- lengths(gold_standard_complexes)
  sizes_pred <- lengths(predicted_complexes)
  
  # Check for protein overlap
  proteins_in_gs <- unique(unlist(gold_standard_complexes))
  overlap_pred_vs_gs_proteins <- vapply(predicted_complexes, function(p_cpx) {
    length(intersect(p_cpx, proteins_in_gs))
  }, integer(1))
  
  if (sum(overlap_pred_vs_gs_proteins) == 0) {
    message("No overlap detected between predicted complex proteins and ",
            "gold standard complex proteins.")
    return(c("ACC" = 0, "Jaccd" = 0, "PR" = 0))
  }
  
  # Calculate metrics using helper functions
  intersection_matrix <- calculate_intersection_matrix(
    gold_standard_complexes, predicted_complexes, num_cores)
  
  # Sensitivity and PPV
  max_inter_gs <- apply(intersection_matrix, 1, max, na.rm = TRUE)
  max_inter_pred <- apply(intersection_matrix, 2, max, na.rm = TRUE)
  
  sensitivity <- calculate_weighted_metric(max_inter_gs, sizes_gs)
  ppv <- calculate_weighted_metric(max_inter_pred, overlap_pred_vs_gs_proteins)
  accuracy <- sqrt(sensitivity * ppv)
  
  # Jaccard index
  jaccard_matrix <- calculate_jaccard_matrix(
    gold_standard_complexes, predicted_complexes, num_cores)
  
  max_jaccard_gs <- apply(jaccard_matrix, 1, max, na.rm = TRUE)
  max_jaccard_pred <- apply(jaccard_matrix, 2, max, na.rm = TRUE)
  
  weighted_jaccard_gs <- calculate_weighted_metric(max_jaccard_gs, sizes_gs)
  weighted_jaccard_pred <- calculate_weighted_metric(max_jaccard_pred, sizes_pred)
  
  jaccard_overall <- if (weighted_jaccard_pred + weighted_jaccard_gs > 0) {
    (2 * weighted_jaccard_pred * weighted_jaccard_gs) /
      (weighted_jaccard_pred + weighted_jaccard_gs)
  } else {
    0
  }
  
  # PR score
  pr_matrix <- calculate_pr_matrix(
    gold_standard_complexes, predicted_complexes, num_cores)
  
  max_pr_gs <- apply(pr_matrix, 1, max, na.rm = TRUE)
  max_pr_pred <- apply(pr_matrix, 2, max, na.rm = TRUE)
  
  weighted_pr_gs <- calculate_weighted_metric(max_pr_gs, sizes_gs)
  weighted_pr_pred <- calculate_weighted_metric(max_pr_pred, sizes_pred)
  
  pr_overall <- if (weighted_pr_pred + weighted_pr_gs > 0) {
    (2 * weighted_pr_gs * weighted_pr_pred) /
      (weighted_pr_gs + weighted_pr_pred)
  } else {
    0
  }
  
  # Return results
  c("ACC" = accuracy, "Jaccd" = jaccard_overall, "PR" = pr_overall)
}
