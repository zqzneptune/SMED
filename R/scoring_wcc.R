#' Score PPIs using Weighted Cross-Correlation (WCC)
#'
#' Calculates Weighted Cross-Correlation (WCC) scores for potential
#' protein-protein interactions (PPIs) based on their elution profiles.
#' WCC is a measure of similarity between time series (elution profiles) that
#' can account for lags.
#'
#' @param elution_matrix A numeric matrix where rows are proteins (named) and
#'   columns are fractions.
#' @param min_fractions_present Integer, minimum number of fractions a protein
#'   must be detected in. Passed to `filter_matrix_by_nonzero_fractions`.
#'   Default is 2.
#' @param window_width Integer, the window width (`trwdth` argument) for `ptw::wcc`.
#'   This defines the maximum lag considered. Default is 1.
#' @param score_cutoff Numeric or `NULL`. If numeric, PPIs with WCC score below
#'   this cutoff are discarded. Applied *before* `top_n_ppi`. Default `NULL`.
#' @param top_n_ppi Integer or `NULL`. If an integer, the top N PPIs by WCC
#'   score (after cutoff) are returned. If `NULL` (default), all PPIs passing
#'   the cutoff are returned.
#'
#' @return A data frame with columns:
#'   \item{PPI}{Protein-protein interaction ("ProteinA~ProteinB").}
#'   \item{wcc_score}{Weighted Cross-Correlation score.}
#'   Sorted by `wcc_score` in descending order.
#' @export
#' @importFrom proxy dist as.matrix
#' @importFrom ptw wcc
#' @seealso \code{\link{filter_matrix_by_nonzero_fractions}},
#'   \code{\link{generate_all_pairwise_ppi}}
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
#'   wcc_scores <- score_ppi_by_wcc(mat, min_fractions_present = 1,
#'                                  window_width = 2, top_n_ppi = 3)
#'   print(wcc_scores)
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
