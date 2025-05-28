#' Utility Functions for SMED Package
#'
#' @description
#' Helper functions used throughout the SMED package for:
#' \itemize{
#'   \item Complex similarity calculations (intersection, Jaccard, PR scores)
#'   \item Elution profile processing and filtering
#'   \item Weighted metric calculations
#' }
#' 
#' @keywords internal
#' @name utils
#' @aliases NULL
NULL

#' Calculate Pairwise Intersections Between Complexes
#'
#' @description
#' Computes a matrix of intersection sizes between all pairs of complexes from two lists.
#' Used internally for complex similarity calculations.
#'
#' @details
#' This function performs all-vs-all comparison of protein complexes using parallel processing.
#' For each pair of complexes (cpx1, cpx2), it calculates the size of their intersection.
#' The result is a matrix where entry [i,j] = length(intersect(cpx1[[i]], cpx2[[j]])).
#'
#' @param complexes1 First list of protein complexes (character vectors)
#' @param complexes2 Second list of protein complexes (character vectors)
#' @param num_cores Number of cores for parallel processing (default = 1)
#' @return Numeric matrix where entry [i,j] contains the size of intersection between
#'         complexes1[[i]] and complexes2[[j]]
#' @keywords internal
#' @seealso \code{\link{calculate_jaccard_matrix}} for similarity metrics,
#'          \code{\link{calculate_pr_matrix}} for precision-recall scores
#' @examples
#' # Compare two sets of complexes
#' cpx1 <- list(c("A", "B", "C"), c("D", "E"))
#' cpx2 <- list(c("A", "B"), c("C", "D", "F"))
#' calculate_intersection_matrix(cpx1, cpx2)
calculate_intersection_matrix <- function(complexes1, complexes2, num_cores = 1) {
  intersection_list <- parallel::mclapply(complexes1, function(cpx1) {
    vapply(complexes2, function(cpx2) {
      length(intersect(cpx1, cpx2))
    }, integer(1))
  }, mc.cores = num_cores)
  
  matrix(
    unlist(intersection_list),
    byrow = TRUE,
    ncol = length(complexes2),
    nrow = length(complexes1)
  )
}

#' Calculate Weighted Metric Scores
#'
#' @description 
#' Computes weighted average of metric values, typically used for complex-level
#' scoring where weights represent complex sizes.
#'
#' @details
#' The weighted average is calculated as sum(metric_values * weights) / sum(weights).
#' If sum(weights) is 0 (all weights zero), returns 0 to avoid division by zero.
#' This is commonly used to calculate size-weighted averages of precision/recall scores.
#'
#' @param metric_values Numeric vector of metric values to weight
#' @param weights Numeric vector of weights (same length as metric_values)
#' @return Weighted average of metric values. Returns 0 if sum of weights is 0.
#' @keywords internal
#' @examples
#' # Weighted average of precision scores by complex size
#' calculate_weighted_metric(c(0.8, 0.9), c(5, 10)) # => 0.8667
#' 
#' # Handles zero weights gracefully
#' calculate_weighted_metric(c(0.5, 0.6), c(0, 0)) # => 0
calculate_weighted_metric <- function(metric_values, weights) {
  if (sum(weights) > 0) {
    sum(metric_values * weights) / sum(weights)
  } else {
    0
  }
}

#' Calculate Pairwise Jaccard Indices
#'
#' @description
#' Computes Jaccard similarity matrix between two lists of protein complexes.
#' Jaccard index measures overlap between sets (intersection/union).
#'
#' @details
#' For each pair of complexes (cpx1, cpx2), calculates:
#' Jaccard = length(intersect(cpx1, cpx2)) / length(union(cpx1, cpx2))
#' Returns 0 if union is empty (both complexes empty).
#' Uses parallel processing for large complex lists.
#'
#' @param complexes1 First list of protein complexes (character vectors)
#' @param complexes2 Second list of protein complexes (character vectors) 
#' @param num_cores Number of cores for parallel processing (default = 1)
#' @return Numeric matrix of Jaccard indices where entry [i,j] contains the
#'         Jaccard similarity between complexes1[[i]] and complexes2[[j]]
#' @keywords internal
#' @seealso \code{\link{calculate_intersection_matrix}} for raw intersection sizes,
#'          \code{\link{calculate_pr_matrix}} for precision-recall scores
#' @examples
#' # Compare complexes using Jaccard index
#' cpx1 <- list(c("A", "B", "C"), c("D", "E"))
#' cpx2 <- list(c("A", "B"), c("C", "D", "F"))
#' calculate_jaccard_matrix(cpx1, cpx2)
calculate_jaccard_matrix <- function(complexes1, complexes2, num_cores = 1) {
  jaccard_list <- parallel::mclapply(complexes1, function(cpx1) {
    vapply(complexes2, function(cpx2) {
      intersect_len <- length(intersect(cpx1, cpx2))
      union_len <- length(union(cpx1, cpx2))
      if (union_len == 0) return(0)
      intersect_len / union_len
    }, numeric(1))
  }, mc.cores = num_cores)
  
  matrix(
    unlist(jaccard_list),
    byrow = TRUE,
    ncol = length(complexes2),
    nrow = length(complexes1)
  )
}

#' Calculate Pairwise PR Scores
#'
#' @description
#' Computes precision-recall (PR) scores between complexes, defined as:
#' (intersection_size^2) / (size1 * size2). Measures both precision and recall.
#'
#' @details
#' The PR score combines precision and recall into single metric:
#' PR = (intersect_size^2)/(len1*len2)
#' Returns 0 if either complex is empty.
#' Uses parallel processing for large complex lists.
#'
#' @param complexes1 First list of protein complexes (character vectors)
#' @param complexes2 Second list of protein complexes (character vectors)
#' @param num_cores Number of cores for parallel processing (default = 1)
#' @return Numeric matrix of PR scores where entry [i,j] contains the
#'         PR score between complexes1[[i]] and complexes2[[j]]
#' @keywords internal
#' @seealso \code{\link{calculate_intersection_matrix}} for raw intersection sizes,
#'          \code{\link{calculate_jaccard_matrix}} for Jaccard similarities
#' @examples
#' # Compare complexes using PR score
#' cpx1 <- list(c("A", "B", "C"), c("D", "E"))
#' cpx2 <- list(c("A", "B"), c("C", "D", "F"))
#' calculate_pr_matrix(cpx1, cpx2)
calculate_pr_matrix <- function(complexes1, complexes2, num_cores = 1) {
  pr_list <- parallel::mclapply(complexes1, function(cpx1) {
    len1 <- length(cpx1)
    if (len1 == 0) return(rep(0, length(complexes2)))
    vapply(complexes2, function(cpx2) {
      len2 <- length(cpx2)
      if (len2 == 0) return(0)
      intersect_len <- length(intersect(cpx1, cpx2))
      (intersect_len^2) / (len2 * len1)
    }, numeric(1))
  }, mc.cores = num_cores)
  
  matrix(
    unlist(pr_list),
    byrow = TRUE,
    ncol = length(complexes2),
    nrow = length(complexes1)
  )
}

#' Prepare Elution Matrix for Analysis
#'
#' @description
#' Preprocesses elution profile matrix by:
#' \itemize{
#'   \item Replacing NAs with 0
#'   \item Adding pseudocount of 1
#' }
#' Ensures matrix is suitable for downstream calculations.
#'
#' @details
#' This preprocessing is critical for:
#' - Avoiding NA propagation in downstream calculations
#' - Preventing division by zero in split ratio calculations
#' The pseudocount (1) ensures non-zero values while preserving relative intensities.
#'
#' @param elution_matrix Numeric matrix of elution profiles (proteins x fractions)
#' @return Processed numeric matrix with same dimensions as input
#' @keywords internal
#' @seealso \code{\link{calculate_split_ratios}} for next processing step
#' @examples
#' # Prepare a sample elution matrix
#' elution <- matrix(c(1, NA, 3, 0, 5, 6), nrow=2)
#' prepare_elution_matrix(elution)
prepare_elution_matrix <- function(elution_matrix) {
  processed_matrix <- elution_matrix
  processed_matrix[is.na(processed_matrix)] <- 0
  processed_matrix + 1
}

#' Calculate Split Ratios for Elution Profiles
#'
#' @description
#' Computes sqrt(control_sum_squares / target_sum_squares) for each protein's
#' elution profile, used to identify proteins with asymmetric elution patterns.
#'
#' @details
#' The split ratio measures asymmetry between two halves of elution profile:
#' 1. Computes sum of squares for control and target halves
#' 2. Takes square root of their ratio
#' Adds small epsilon (.Machine$double.eps) to denominator to avoid division by zero.
#'
#' @param processed_matrix Prepared elution matrix from \code{\link{prepare_elution_matrix}}
#' @param control_indices Column indices for control half of elution profile
#' @param target_indices Column indices for target half of elution profile
#' @return Numeric vector of split ratios (one per protein)
#' @keywords internal
#' @seealso \code{\link{filter_by_mad_threshold}} for filtering based on ratios
#' @examples
#' # Calculate split ratios for sample data
#' elution <- matrix(1:12, nrow=3)
#' prepare_elution_matrix(elution) |>
#'   calculate_split_ratios(1:2, 3:4)
calculate_split_ratios <- function(processed_matrix, control_indices, target_indices) {
  sum_sq_control <- rowSums(processed_matrix[, control_indices, drop=FALSE]^2)
  sum_sq_target <- rowSums(processed_matrix[, target_indices, drop=FALSE]^2)
  sqrt(sum_sq_control / (sum_sq_target + .Machine$double.eps))
}

#' Filter Proteins by MAD Threshold
#'
#' @description
#' Identifies proteins whose split ratios deviate from median by specified number
#' of MADs (median absolute deviations). Used to find proteins with asymmetric
#' elution patterns.
#'
#' @details
#' Filtering process:
#' 1. Computes median and MAD of split ratios
#' 2. If MAD is 0 (all ratios identical), returns proteins with non-median ratios
#' 3. Otherwise, returns proteins outside median ± (threshold * MAD)
#'
#' @param mean_ratios_per_protein Named numeric vector of mean split ratios
#' @param mad_threshold Number of MADs from median to use as cutoff (default = 2.5)
#' @return Character vector of protein names passing filter
#' @keywords internal
#' @seealso \code{\link{calculate_split_ratios}} for computing input ratios
#' @examples
#' # Filter proteins with asymmetric elution
#' ratios <- c(A=1.2, B=0.8, C=3.5, D=1.1)
#' filter_by_mad_threshold(ratios, mad_threshold=2)
filter_by_mad_threshold <- function(mean_ratios_per_protein, mad_threshold = 2.5) {
  median_of_ratios <- stats::median(mean_ratios_per_protein, na.rm = TRUE)
  mad_of_ratios <- stats::mad(mean_ratios_per_protein, na.rm = TRUE)
  
  if (mad_of_ratios == 0) {
    return(names(mean_ratios_per_protein)[mean_ratios_per_protein != median_of_ratios])
  }
  
  lower_bound <- median_of_ratios - mad_threshold * mad_of_ratios
  upper_bound <- median_of_ratios + mad_threshold * mad_of_ratios
  
  names(mean_ratios_per_protein)[
    (mean_ratios_per_protein < lower_bound) |
      (mean_ratios_per_protein > upper_bound)
  ]
}