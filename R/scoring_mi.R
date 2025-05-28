#' Score PPIs using Mutual Information from Elution Profiles
#'
#' Calculates Mutual Information (MI) scores for potential protein-protein
#' interactions (PPIs) based on co-elution patterns. MI measures the non-linear
#' dependence between protein elution profiles, capturing both linear and 
#' non-linear correlations that may indicate functional relationships or physical
#' interactions.
#'
#' @details 
#' Biological Significance:
#' Mutual information quantifies how much knowing the elution profile of one 
#' protein reduces uncertainty about another's profile. High MI suggests:
#' \itemize{
#'   \item Potential physical interaction in the same complex
#'   \item Functional coordination (e.g., same pathway)
#'   \item Shared regulatory mechanisms
#' }
#' Compared to linear metrics like Pearson correlation, MI can detect more complex
#' relationships but may be more sensitive to noise.
#'
#' Calculation Process:
#' 1. Filter proteins by minimum presence (removes sparse profiles)
#' 2. Discretize continuous elution profiles into bins
#' 3. Compute pairwise MI using empirical probability estimates
#' 4. Filter and rank interactions by MI score
#'
#' Discretization:
#' Continuous elution profiles are converted to discrete values using:
#' \itemize{
#'   \item \code{equalwidth}: Uniform bin widths (default)
#'   \item \code{equalfreq}: Equal observations per bin
#'   \item \code{globalequalwidth}: Global binning across all proteins
#' }
#' More bins increase sensitivity but require more data. Typical values are 3-5 bins.
#'
#' Edge Cases:
#' \itemize{
#'   \item Returns empty data frame if <2 proteins pass filtering
#'   \item Handles NA values by imputation with 0
#'   \item Validates matrix structure and names
#' }
#'
#' @param elution_matrix A numeric matrix where rows are proteins (named) and
#'   columns are fractions. Should contain raw or normalized intensity values.
#' @param min_fractions_present Integer, minimum number of fractions a protein
#'   must be detected in. Higher values increase stringency but may reduce
#'   coverage. Passed to `filter_matrix_by_nonzero_fractions`. Default is 0.
#' @param discretization_method Character, method for discretizing continuous
#'   elution data. Passed to `infotheo::discretize`. Common choices are:
#'   \itemize{
#'     \item "equalwidth" (default): Uniform bin widths
#'     \item "equalfreq": Equal observations per bin  
#'     \item "globalequalwidth": Global uniform binning
#'   }
#' @param num_bins Integer, number of bins for discretization (typically 3-5).
#'   More bins capture finer patterns but require more data. Default 3.
#' @param mi_method Character, method for MI estimation. Passed to
#'   `infotheo::mutinformation`. Options include:
#'   \itemize{
#'     \item "emp" (default): Empirical estimator
#'     \item "mm": Miller-Madow correction
#'     \item "shrink": Shrinkage estimator
#'   }
#' @param score_cutoff Numeric or `NULL`. If numeric, PPIs with MI score below
#'   this cutoff are discarded. Higher values increase precision but reduce
#'   recall. Applied before `top_n_ppi`. Default `NULL`.
#' @param top_n_ppi Integer or `NULL`. If an integer, returns the top N PPIs by
#'   MI score (after cutoff). If `NULL` (default), returns all PPIs passing
#'   the cutoff.
#'
#' @return A data frame with columns:
#'   \item{PPI}{Protein-protein interaction ("ProteinA~ProteinB")}
#'   \item{mi_score}{Mutual Information score (bits)}
#'   Sorted by `mi_score` in descending order.
#'
#' @export
#' @importFrom infotheo discretize mutinformation
#' @seealso \code{\link{filter_matrix_by_nonzero_fractions}},
#'   \code{\link{generate_all_pairwise_ppi}}, \code{\link{score_ppi_by_mi.Rd}}
#' @examples
#' if (requireNamespace("infotheo", quietly = TRUE)) {
#'   set.seed(123)
#'   mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#'   rownames(mat) <- paste0("P", 1:10)
#'   # Add some correlation for P1, P2
#'   mat["P2",] <- mat["P1",] + rnorm(10, 0, 0.5)
#'   mat[1:5,1] <- 1 # for min_fractions_present
#'
#'   # Basic usage with equal-width discretization
#'   mi_scores <- score_ppi_by_mi(mat, min_fractions_present = 1,
#'                                num_bins = 3, top_n_ppi = 5)
#'   print(mi_scores)
#'
#'   # More bins with equal-frequency discretization
#'   mi_scores2 <- score_ppi_by_mi(mat, discretization_method = "equalfreq",
#'                                num_bins = 5)
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
