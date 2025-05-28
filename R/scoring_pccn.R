#' Score PPIs using Poisson Correlation Coefficient of Noise (PCCN)
#'
#' Calculates robust Pearson correlation scores for potential protein-protein 
#' interactions (PPIs) by adding Poisson noise to elution profiles and averaging 
#' correlations across multiple replicates. This approach helps identify stable 
#' co-elution patterns in count-like data while accounting for measurement noise.
#'
#' @details
#' The PCCN method works through these key steps:
#' \enumerate{
#'   \item \strong{Noise Addition}: Adds Poisson noise to each value in the 
#'   elution matrix (rpois) to simulate technical variability
#'   \item \strong{Normalization}: Applies compositional normalization by:
#'     \itemize{
#'       \item Adding a small pseudocount (1/num_fractions) to avoid zeros
#'       \item Normalizing each protein's profile to sum to 1 (relative abundance)
#'     }
#'   \item \strong{Correlation Calculation}: Computes Pearson correlation between 
#'   all protein pairs on the noise-added, normalized data
#'   \item \strong{Averaging}: Repeats steps 1-3 for multiple replicates and 
#'   averages the correlation scores
#' }
#'
#' \strong{Biological Interpretation}: Higher PCCN scores indicate proteins that:
#' \itemize{
#'   \item Co-elute consistently across fractions
#'   \item Maintain correlation patterns despite noise
#'   \item Are more likely to interact or be part of same complex
#' }
#' Scores range from -1 (perfect anti-correlation) to 1 (perfect correlation), 
#' with values near 0 indicating no relationship.
#'
#' \strong{Comparison to Other Metrics}:
#' \itemize{
#'   \item More robust to noise than standard Pearson correlation
#'   \item Better for count data than WCC (works with zeros)
#'   \item Less sensitive to extreme values than MI
#'   \item Captures linear relationships unlike Dice
#' }
#'
#' \strong{Edge Cases Handled}:
#' \itemize{
#'   \item Proteins with constant values after normalization (var=0)
#'   \item Negative input values (with warning)
#'   \item Fewer than 2 proteins after filtering
#'   \item NA values (imputed as 0 before calculation)
#' }
#'
#' @param elution_matrix A numeric matrix where rows are proteins (named) and
#'   columns are fractions. Values should ideally be counts or pseudo-counts.
#'   Negative values will be problematic for `rpois`.
#' @param min_fractions_present Integer, minimum number of fractions a protein
#'   must be detected in. Higher values increase stringency but may reduce 
#'   coverage. Default 2 provides balance.
#' @param num_replicates Integer, number of replications for adding noise and
#'   calculating correlations. More replicates increase stability but slow 
#'   computation. Default 10 provides good balance.
#' @param score_cutoff Numeric or `NULL`. If numeric, PPIs with PCCN score below
#'   this cutoff are discarded. Applied before `top_n_ppi`. Default `NULL`.
#' @param top_n_ppi Integer or `NULL`. If an integer, the top N PPIs by PCCN
#'   score (after cutoff) are returned. If `NULL` (default), all PPIs passing
#'   the cutoff are returned.
#' @param verbose Logical, if `TRUE`, a progress bar is shown. Default `TRUE`.
#'
#' @return A data frame with columns:
#'   \item{PPI}{Protein-protein interaction ("ProteinA~ProteinB").}
#'   \item{pccn_score}{Averaged PCCN score (Pearson correlation with noise).}
#'   Sorted by `pccn_score` in descending order.
#' @export
#' @importFrom stats cor rpois
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @seealso \code{\link{filter_matrix_by_nonzero_fractions}},
#'   \code{\link{generate_all_pairwise_ppi}}, \code{\link{score_ppi_by_pccn.Rd}}
#' @examples
#' set.seed(123)
#' # Create count-like data
#' mat_counts <- matrix(rpois(5 * 10, lambda = 5), nrow = 5, ncol = 10)
#' rownames(mat_counts) <- paste0("P", 1:5)
#' # Add some correlation for P1, P2 by making P2 similar to P1
#' mat_counts["P2",] <- mat_counts["P1",] + rpois(10, lambda = 1)
#'
#' pccn_scores <- score_ppi_by_pccn(mat_counts, min_fractions_present = 1,
#'                                  num_replicates = 5, # Fewer for example
#'                                  top_n_ppi = 3, verbose = FALSE)
#' print(pccn_scores)
score_ppi_by_pccn <- function(elution_matrix,
                             min_fractions_present = 2,
                             num_replicates = 10,
                             score_cutoff = NULL,
                             top_n_ppi = NULL,
                             verbose = TRUE) {

  if (!is.matrix(elution_matrix) || !is.numeric(elution_matrix)) {
    stop("'elution_matrix' must be a numeric matrix.")
  }
  if (is.null(rownames(elution_matrix))) {
    stop("'elution_matrix' must have row names (protein identifiers).")
  }
  if (any(elution_matrix < 0, na.rm = TRUE)) {
    warning("Input matrix contains negative values.",
            " 'rpois' expects non-negative lambda. Results may be unreliable.")
  }

  sorted_row_names <- sort(rownames(elution_matrix))
  elution_matrix_sorted <- elution_matrix[sorted_row_names, , drop = FALSE]

  filtered_elution_matrix <- filter_matrix_by_nonzero_fractions(
    elution_matrix_sorted,
    min_fractions = min_fractions_present
  )

  if (nrow(filtered_elution_matrix) < 2) {
    message("Less than 2 proteins after filtering, cannot calculate PCCN scores.")
    return(data.frame(PPI = character(0), pccn_score = numeric(0)))
  }

  # Impute NAs with 0 before PCCN calculation
  active_matrix <- filtered_elution_matrix
  active_matrix[is.na(active_matrix)] <- 0

  num_proteins <- nrow(active_matrix)
  num_fractions <- ncol(active_matrix)
  accumulated_cor_matrix <- matrix(0, nrow = num_proteins, ncol = num_proteins)

  if (verbose) {
    message("Computing PCCN scores...")
    pb <- utils::txtProgressBar(min = 0, max = num_replicates, style = 3)
  }

  for (i in seq_len(num_replicates)) {
    # Add Poisson noise: rpois applied element-wise to active_matrix
    # Ensure lambda is not negative if active_matrix had negatives (already warned)
    noisy_matrix_A <- apply(active_matrix, c(1,2),
                            function(x) stats::rpois(1, lambda = max(0, x)))

    # Add pseudocount (1/num_fractions) and normalize rows to sum to 1
    # (compositional data)
    noisy_matrix_C <- noisy_matrix_A + (1 / num_fractions)
    row_sums_C <- rowSums(noisy_matrix_C)
    # Avoid division by zero if a row sum is zero
    row_sums_C[row_sums_C == 0] <- 1
    compositional_matrix_B <- noisy_matrix_C / row_sums_C

    # Calculate Pearson correlation (proteins are rows, so transpose)
    # pairwise.complete.obs handles potential NAs if any row becomes all same
    # after normalization (though unlikely with pseudocount)
    # Check for constant rows that would cause correlation warnings
    row_vars <- apply(compositional_matrix_B, 1, var, na.rm = TRUE)
    if (any(row_vars == 0, na.rm = TRUE)) {
      const_proteins <- rownames(compositional_matrix_B)[which(row_vars == 0)]
      message("Some proteins have constant values after normalization (",
              paste(const_proteins, collapse = ", "),
              "). Their correlations will be NA.")
    }

    # Calculate correlations with explicit handling
    cor_matrix_B <- tryCatch(
      stats::cor(t(compositional_matrix_B), use = "pairwise.complete.obs"),
      warning = function(w) {
        message("Correlation calculation: ", w$message)
        stats::cor(t(compositional_matrix_B), use = "pairwise.complete.obs")
      }
    )
    cor_matrix_B[is.na(cor_matrix_B)] <- 0 # Replace NAs from cor if any

    accumulated_cor_matrix <- accumulated_cor_matrix + cor_matrix_B
    if (verbose) utils::setTxtProgressBar(pb, i)
  }
  if (verbose) close(pb)

  avg_pccn_matrix <- accumulated_cor_matrix / num_replicates
  rownames(avg_pccn_matrix) <- colnames(avg_pccn_matrix) <-
    rownames(active_matrix)

  pccn_scores_vector <-
    avg_pccn_matrix[lower.tri(avg_pccn_matrix, diag = FALSE)]

  # Generate PPI identifiers
  ppi_df <- generate_all_pairwise_ppi(rownames(avg_pccn_matrix))

  if (nrow(ppi_df) != length(pccn_scores_vector)) {
    stop("Mismatch between number of PPIs and calculated PCCN scores.",
         " This indicates an issue with matrix processing or PPI generation.")
  }
  ppi_df$pccn_score <- pccn_scores_vector

  # Filter by score_cutoff if provided
  if (!is.null(score_cutoff)) {
    ppi_df <- ppi_df[ppi_df$pccn_score >= score_cutoff, , drop = FALSE]
  }

  # Sort by PCCN score (descending)
  final_ppi_df <- ppi_df[order(ppi_df$pccn_score, decreasing = TRUE), ]
  rownames(final_ppi_df) <- NULL

  # Select top N PPIs if requested
  if (!is.null(top_n_ppi) && nrow(final_ppi_df) > top_n_ppi) {
    final_ppi_df <- final_ppi_df[seq_len(top_n_ppi), ]
  }

  return(final_ppi_df[, c("PPI", "pccn_score")])
}
