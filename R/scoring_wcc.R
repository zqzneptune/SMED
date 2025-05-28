#' Score PPIs using Weighted Cross-Correlation (WCC)
#'
#' Calculates Weighted Cross-Correlation (WCC) scores for potential protein-protein 
#' interactions (PPIs) based on their co-elution profiles. WCC is particularly 
#' useful for detecting shifted or lagged relationships between protein elution 
#' profiles, which may indicate transient interactions or complex formation 
#' dynamics.
#'
#' @details
#' The Weighted Cross-Correlation (WCC) method implemented here uses the 
#' `ptw::wcc` function which calculates a similarity score between two profiles 
#' considering possible lags. Key features:
#' \itemize{
#'   \item \strong{Biological Significance}: WCC can detect protein pairs that 
#'   co-elute but with potential time shifts, which may occur when proteins 
#'   interact transiently or as part of sequential complex assembly.
#'   \item \strong{Weighting Approach}: The WCC calculation uses a triangular 
#'   weighting function centered at zero lag, giving highest weight to perfect 
#'   co-elution while still considering shifted profiles. The `window_width` 
#'   parameter controls how far to look for shifted matches.
#'   \item \strong{Comparison to Other Metrics}: Unlike Pearson Correlation 
#'   (PCC), WCC can detect lagged relationships. Compared to Mutual Information 
#'   (MI), WCC is more sensitive to linear relationships but less robust to 
#'   non-linear patterns.
#' }
#'
#' @param elution_matrix A numeric matrix where rows are proteins (named) and
#'   columns are fractions. Missing values should be NA. Profiles should be 
#'   normalized (e.g., by total ion count) before WCC calculation.
#' @param min_fractions_present Integer, minimum number of fractions a protein
#'   must be detected in (non-NA). Proteins with fewer detections are filtered 
#'   out. Default 2 allows minimal detection while avoiding single-point 
#'   artifacts. Higher values increase stringency but may reduce coverage.
#' @param window_width Integer, the maximum lag considered (trwdth parameter 
#'   in `ptw::wcc`). Larger values allow detection of more shifted relationships 
#'   but increase noise sensitivity. Default 1 balances sensitivity and 
#'   specificity for typical SEC experiments.
#' @param score_cutoff Numeric or `NULL`. If numeric, PPIs with WCC score below
#'   this value are discarded. Typical cutoffs range 0.3-0.7 depending on data 
#'   quality and window_width. Applied before `top_n_ppi`. Default `NULL`.
#' @param top_n_ppi Integer or `NULL`. If specified, returns only the top N 
#'   highest-scoring PPIs. Useful for reducing computational burden in 
#'   downstream analyses. Default `NULL` returns all PPIs above cutoff.
#'
#' @return A data frame with columns:
#'   \item{PPI}{Protein-protein interaction in "ProteinA~ProteinB" format.}
#'   \item{wcc_score}{Weighted Cross-Correlation score ranging from -1 (perfect 
#'   anti-correlation) to 1 (perfect correlation), with higher absolute values 
#'   indicating stronger relationships. Scores near 0 suggest no relationship.}
#'   The data frame is sorted by `wcc_score` in descending order.
#'
#' @section Edge Cases and Special Handling:
#' \itemize{
#'   \item Proteins with < `min_fractions_present` detections are filtered out
#'   \item NA values are imputed as 0 before WCC calculation
#'   \item If < 2 proteins remain after filtering, returns empty data frame
#'   \item WCC calculation errors (e.g., constant profiles) return NA scores
#' }
#'
#' @export
#' @importFrom proxy dist as.matrix
#' @importFrom ptw wcc
#' @seealso \code{\link{filter_matrix_by_nonzero_fractions}},
#'   \code{\link{generate_all_pairwise_ppi}}, \code{\link{score_ppi_by_pccn}},
#'   \code{\link{score_ppi_by_mi}}
#' @examples
#' if (requireNamespace("proxy", quietly = TRUE) &&
#'     requireNamespace("ptw", quietly = TRUE)) {
#'   set.seed(123)
#'   mat <- matrix(rnorm(5 * 10), nrow = 5, ncol = 10)
#'   rownames(mat) <- paste0("P", 1:5)
#'   # P2 is a shifted version of P1
#'   mat["P2", 2:10] <- mat["P1", 1:9]
#'   mat[1:5,1] <- 1 # for min_fractions
#'
#'   # Basic usage
#'   wcc_scores <- score_ppi_by_wcc(mat, min_fractions_present = 1)
#'   
#'   # Demonstrate lag detection
#'   wide_window <- score_ppi_by_wcc(mat, min_fractions_present = 1,
#'                                  window_width = 3)
#'   print(wide_window[wide_window$PPI == "P1~P2",]) # Should show high score
#' }
score_ppi_by_wcc <- function(elution_matrix,
                             min_fractions_present = 2,
                             window_width = 1,
                             score_cutoff = NULL,
                             top_n_ppi = NULL) {

  if (!is.matrix(elution_matrix) || !is.numeric(elution_matrix)) {
    stop("'elution_matrix' must be a numeric matrix.")
  }
  if (is.null(rownames(elution_matrix))) {
    stop("'elution_matrix' must have row names (protein identifiers).")
  }
  if (!requireNamespace("proxy", quietly = TRUE)) {
    stop("Package 'proxy' needed for this function. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("ptw", quietly = TRUE)) {
    stop("Package 'ptw' needed for ptw::wcc. Please install it.",
         call. = FALSE)
  }


  sorted_row_names <- sort(rownames(elution_matrix))
  elution_matrix_sorted <- elution_matrix[sorted_row_names, , drop = FALSE]

  filtered_elution_matrix <- filter_matrix_by_nonzero_fractions(
    elution_matrix_sorted,
    min_fractions = min_fractions_present
  )

  if (nrow(filtered_elution_matrix) < 2) {
    message("Less than 2 proteins after filtering, cannot calculate WCC scores.")
    return(data.frame(PPI = character(0), wcc_score = numeric(0)))
  }

  # Impute NAs with 0 before WCC calculation
  active_matrix <- filtered_elution_matrix
  active_matrix[is.na(active_matrix)] <- 0

  # Define the WCC function for proxy::dist
  # ptw::wcc returns a similarity score (higher is better)
  wcc_similarity_func <- function(x, y) {
    tryCatch(
      ptw::wcc(x, y, trwdth = window_width),
      error = function(e) NA_real_ # Handle errors in ptw::wcc if they occur
    )
  }

  # proxy::dist computes distances. To get similarities directly,
  # one might use proxy::simil or ensure the function returns 1-similarity.
  # However, the original code used dist and then sorted descending on the
  # "distance" which was actually a similarity. We'll keep that structure
  # meaning wcc_matrix stores similarities.
  # proxy::dist with a custom function that returns similarity works if we
  # interpret the result matrix as a similarity matrix.
  wcc_matrix <- proxy::dist(active_matrix, method = wcc_similarity_func)
  wcc_matrix <- as.matrix(wcc_matrix) # Convert dist object to matrix

  wcc_scores_vector <- wcc_matrix[lower.tri(wcc_matrix, diag = FALSE)]

  # Generate PPI identifiers
  ppi_df <- generate_all_pairwise_ppi(rownames(wcc_matrix))

  if (nrow(ppi_df) != length(wcc_scores_vector)) {
    stop("Mismatch between number of PPIs and calculated WCC scores.",
         " This indicates an issue with matrix processing or PPI generation.")
  }
  ppi_df$wcc_score <- wcc_scores_vector

  # Filter by score_cutoff if provided
  if (!is.null(score_cutoff)) {
    ppi_df <- ppi_df[ppi_df$wcc_score >= score_cutoff, , drop = FALSE]
  }

  # Sort by WCC score (descending)
  final_ppi_df <- ppi_df[order(ppi_df$wcc_score, decreasing = TRUE), ]
  rownames(final_ppi_df) <- NULL

  # Select top N PPIs if requested
  if (!is.null(top_n_ppi) && nrow(final_ppi_df) > top_n_ppi) {
    final_ppi_df <- final_ppi_df[seq_len(top_n_ppi), ]
  }

  return(final_ppi_df[, c("PPI", "wcc_score")])
}
