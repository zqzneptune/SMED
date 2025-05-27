#' Score PPIs using Dice Coefficient from Elution Profiles
#'
#' Calculates the Dice similarity coefficient for potential protein-protein
#' interactions (PPIs) based on their co-occurrence in elution fractions.
#' The matrix is binarized (presence/absence in a fraction), and then the Dice
#' coefficient is computed for each pair of proteins.
#'
#' @param elution_matrix A numeric matrix where rows are proteins (named) and
#'   columns are fractions.
#' @param min_fractions_present Integer, minimum number of fractions a protein
#'   must be detected in. Passed to `filter_matrix_by_nonzero_fractions`.
#'   Default is 0.
#' @param top_n_ppi Integer or `NULL`. If an integer, the top N PPIs by Dice
#'   score are returned. If `NULL` (default), all scored PPIs are returned.
#'
#' @return A data frame with columns:
#'   \item{PPI}{Protein-protein interaction identifier ("ProteinA~ProteinB").}
#'   \item{dice_similarity_score}{Dice similarity coefficient (0-1), where
#'     higher scores indicate more similar elution patterns (co-occurrence).}
#'   The data frame is sorted by `dice_similarity_score` in descending order.
#' @export
#' @importFrom arules dissimilarity
#' @seealso \code{\link{filter_matrix_by_nonzero_fractions}},
#'   \code{\link{generate_all_pairwise_ppi}}
#' @examples
#' set.seed(123)
#' mat <- matrix(0, nrow = 3, ncol = 5)
#' rownames(mat) <- c("P1", "P2", "P3")
#' mat["P1", 1:3] <- 1      # P1 in F1,F2,F3
#' mat["P2", 2:4] <- 1      # P2 in F2,F3,F4 (overlap with P1 in F2,F3)
#' mat["P3", 4:5] <- 1      # P3 in F4,F5 (overlap with P2 in F4)
#'
#' dice_scores <- score_ppi_by_dice(mat, min_fractions_present = 1, top_n_ppi = 3)
#' print(dice_scores)
score_ppi_by_dice <- function(elution_matrix,
                              min_fractions_present = 0,
                              top_n_ppi = NULL) {

  if (!is.matrix(elution_matrix) || !is.numeric(elution_matrix)) {
    stop("'elution_matrix' must be a numeric matrix.")
  }
  if (is.null(rownames(elution_matrix))) {
    stop("'elution_matrix' must have row names (protein identifiers).")
  }

  # Ensure matrix is sorted by row names for consistent PPI generation
  # This is important if generate_all_pairwise_ppi relies on sorted input
  # for its internal sorting logic, although it sorts unique proteins anyway.
  # It's good practice for consistency of matrix before dissimilarity.
  sorted_row_names <- sort(rownames(elution_matrix))
  elution_matrix_sorted <- elution_matrix[sorted_row_names, , drop = FALSE]


  filtered_elution_matrix <- filter_matrix_by_nonzero_fractions(
    elution_matrix_sorted,
    min_fractions = min_fractions_present
  )

  if (nrow(filtered_elution_matrix) < 2) {
    message("Less than 2 proteins after filtering, cannot calculate Dice scores.")
    return(data.frame(PPI = character(0),
                      dice_similarity_score = numeric(0)))
  }

  # Binarize matrix: presence (1) or absence (0) in fractions
  # NAs are treated as 0 by filter_matrix... but for binarization, ensure they are 0.
  binary_matrix <- filtered_elution_matrix
  binary_matrix[is.na(binary_matrix)] <- 0
  binary_matrix <- ifelse(binary_matrix > 0, 1, 0)

  # Calculate Dice dissimilarity using arules
  # method = "dice" in arules::dissimilarity calculates 1 - Dice_coefficient
  # So, a small value means high similarity.
  # The matrix needs to be logical or 0/1 for "dice" method in older arules.
  # Newer arules might handle numeric, but explicit conversion is safer.
  if (!all(binary_matrix %in% c(0,1))) {
    # This should not happen if ifelse worked correctly.
    stop("Binary matrix preparation failed. Matrix should contain only 0 or 1.")
  }
  # arules expects items in rows, features in columns for transactions,
  # but for dissimilarity on a matrix, it treats rows as items.
  dice_dist_matrix <- suppressWarnings(as.matrix(
    arules::dissimilarity(binary_matrix, method = "dice")
  ))

  # Convert Dice distance to Dice similarity
  dice_similarity_values <- 1 - dice_dist_matrix[
    lower.tri(dice_dist_matrix, diag = FALSE)
  ]

  # Generate PPI identifiers for the lower triangle
  ppi_df <- generate_all_pairwise_ppi(rownames(dice_dist_matrix))

  if (nrow(ppi_df) != length(dice_similarity_values)) {
    stop("Mismatch between number of PPIs and calculated Dice scores.",
         " This indicates an issue with matrix processing or PPI generation.")
  }
  ppi_df$dice_similarity_score <- dice_similarity_values

  # Sort by Dice similarity score (descending)
  final_ppi_df <- ppi_df[
    order(ppi_df$dice_similarity_score, decreasing = TRUE),
  ]
  rownames(final_ppi_df) <- NULL

  # Select top N PPIs if requested
  if (!is.null(top_n_ppi) && nrow(final_ppi_df) > top_n_ppi) {
    final_ppi_df <- final_ppi_df[seq_len(top_n_ppi), ]
  }

  return(final_ppi_df[, c("PPI", "dice_similarity_score")])
}
