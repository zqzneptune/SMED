#' Score PPIs using Mutual Information from Elution Profiles
#'
#' Calculates Mutual Information (MI) scores for potential protein-protein
#' interactions (PPIs). Elution profiles are first discretized, and then
#' pairwise MI is computed. Higher MI suggests stronger (non-linear) correlation.
#'
#' @param elution_matrix A numeric matrix where rows are proteins (named) and
#'   columns are fractions.
#' @param min_fractions_present Integer, minimum number of fractions a protein
#'   must be detected in. Passed to `filter_matrix_by_nonzero_fractions`.
#'   Default is 0.
#' @param discretization_method Character, method for discretizing continuous
#'   elution data. Passed to `infotheo::discretize`. Common choices are
#'   "globalequalwidth", "equalwidth", "equalfreq". Default "equalwidth".
#' @param num_bins Integer, number of bins for discretization. Default 3.
#' @param mi_method Character, method for MI estimation. Passed to
#'   `infotheo::mutinformation`. E.g., "emp" (empirical estimator).
#'   Default "emp".
#' @param score_cutoff Numeric or `NULL`. If numeric, PPIs with MI score below
#'   this cutoff are discarded. Applied *before* `top_n_ppi`. Default `NULL`.
#' @param top_n_ppi Integer or `NULL`. If an integer, the top N PPIs by MI
#'   score (after cutoff) are returned. If `NULL` (default), all PPIs passing
#'   the cutoff are returned.
#'
#' @return A data frame with columns:
#'   \item{PPI}{Protein-protein interaction ("ProteinA~ProteinB").}
#'   \item{mi_score}{Mutual Information score.}
#'   Sorted by `mi_score` in descending order.
#' @export
#' @importFrom infotheo discretize mutinformation
#' @seealso \code{\link{filter_matrix_by_nonzero_fractions}},
#'   \code{\link{generate_all_pairwise_ppi}}
#' @examples
#' if (requireNamespace("infotheo", quietly = TRUE)) {
#'   set.seed(123)
#'   mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#'   rownames(mat) <- paste0("P", 1:10)
#'   # Add some correlation for P1, P2
#'   mat["P2",] <- mat["P1",] + rnorm(10, 0, 0.5)
#'   mat[1:5,1] <- 1 # for min_fractions_present
#'
#'   mi_scores <- score_ppi_by_mi(mat, min_fractions_present = 1,
#'                                num_bins = 3, top_n_ppi = 5)
#'   print(mi_scores)
#' }
score_ppi_by_mi <- function(elution_matrix,
                            min_fractions_present = 0,
                            discretization_method = "equalwidth",
                            num_bins = 3,
                            mi_method = "emp",
                            score_cutoff = NULL,
                            top_n_ppi = NULL) {

  if (!is.matrix(elution_matrix) || !is.numeric(elution_matrix)) {
    stop("'elution_matrix' must be a numeric matrix.")
  }
  if (is.null(rownames(elution_matrix))) {
    stop("'elution_matrix' must have row names (protein identifiers).")
  }
  if (!requireNamespace("infotheo", quietly = TRUE)) {
    stop("Package 'infotheo' needed for this function to work.",
         " Please install it.", call. = FALSE)
  }

  sorted_row_names <- sort(rownames(elution_matrix))
  elution_matrix_sorted <- elution_matrix[sorted_row_names, , drop = FALSE]

  filtered_elution_matrix <- filter_matrix_by_nonzero_fractions(
    elution_matrix_sorted,
    min_fractions = min_fractions_present
  )

  if (nrow(filtered_elution_matrix) < 2) {
    message("Less than 2 proteins after filtering, cannot calculate MI scores.")
    return(data.frame(PPI = character(0), mi_score = numeric(0)))
  }

  # Impute NAs with 0 before discretization
  filtered_elution_matrix[is.na(filtered_elution_matrix)] <- 0

  # Discretize data: infotheo expects variables in columns, samples in rows.
  # Our matrix has proteins (variables for MI) in rows, fractions (samples) in cols.
  # So, we need to transpose it.
  discretized_matrix <- infotheo::discretize(
    X = t(filtered_elution_matrix),
    disc = discretization_method,
    nbins = num_bins
  )
  # Result has proteins as columns.

  # Calculate pairwise Mutual Information
  # mutinformation expects variables in columns.
  mi_matrix <- infotheo::mutinformation(discretized_matrix, method = mi_method)
  # mi_matrix is symmetric, proteins are in rows and columns.

  mi_scores_vector <- mi_matrix[lower.tri(mi_matrix, diag = FALSE)]

  # Generate PPI identifiers
  ppi_df <- generate_all_pairwise_ppi(rownames(mi_matrix)) # or colnames

  if (nrow(ppi_df) != length(mi_scores_vector)) {
    stop("Mismatch between number of PPIs and calculated MI scores.",
         " This indicates an issue with matrix processing or PPI generation.")
  }
  ppi_df$mi_score <- mi_scores_vector

  # Filter by score_cutoff if provided
  if (!is.null(score_cutoff)) {
    ppi_df <- ppi_df[ppi_df$mi_score >= score_cutoff, , drop = FALSE]
  }

  # Sort by MI score (descending)
  final_ppi_df <- ppi_df[order(ppi_df$mi_score, decreasing = TRUE), ]
  rownames(final_ppi_df) <- NULL

  # Select top N PPIs if requested
  if (!is.null(top_n_ppi) && nrow(final_ppi_df) > top_n_ppi) {
    final_ppi_df <- final_ppi_df[seq_len(top_n_ppi), ]
  }

  return(final_ppi_df[, c("PPI", "mi_score")])
}
