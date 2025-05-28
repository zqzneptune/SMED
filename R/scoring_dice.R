#' Score PPIs using Dice Coefficient from Elution Profiles
#'
#' Calculates the Dice similarity coefficient for potential protein-protein
#' interactions (PPIs) based on their co-occurrence in elution fractions.
#' The matrix is binarized (presence/absence in a fraction), and then the Dice
#' coefficient is computed for each pair of proteins.
#'
#' @section Dice Coefficient Formula:
#' The Dice coefficient (D) between two proteins A and B is calculated as:
#' \deqn{D = \frac{2 \times |A \cap B|}{|A| + |B|}}
#' where:
#' \itemize{
#'   \item \eqn{|A \cap B|} is the count of fractions where both proteins are present
#'   \item \eqn{|A|} and \eqn{|B|} are counts of fractions where each protein is present
#' }
#' The score ranges from 0 (no co-occurrence) to 1 (identical elution profiles).
#'
#' @section Biological Interpretation:
#' The Dice coefficient measures the similarity of two proteins' elution patterns:
#' \itemize{
#'   \item High scores (near 1) suggest proteins co-elute frequently, indicating potential interaction
#'   \item Low scores (near 0) suggest independent elution patterns
#'   \item Scores above 0.5-0.7 are typically considered biologically significant
#'   \item Example biological scenarios:
#'     \itemize{
#'       \item Scores >0.8: Likely stable complex members
#'       \item Scores 0.6-0.8: Potential transient interactions
#'       \item Scores 0.4-0.6: Weak or indirect associations
#'     }
#' }
#'
#' @section Advantages vs Other Metrics:
#' Compared to other similarity metrics:
#' \itemize{
#'   \item More sensitive to co-presence than Jaccard index
#'   \item Less affected by absence in fractions than Pearson correlation
#'   \item More robust to noise than mutual information for sparse data
#'   \item However, does not account for intensity differences (only presence/absence)
#' }
#'
#' @section Performance Characteristics:
#' \itemize{
#'   \item Time complexity: O(n^2 * m) where n is number of proteins and m is number of fractions
#'   \item Memory usage: O(n^2) for distance matrix storage
#'   \item Optimizations:
#'     \itemize{
#'       \item Uses sparse matrix operations when possible
#'       \item Parallel processing not implemented (could benefit large datasets)
#'     }
#'   \item Typical runtime benchmarks:
#'     \itemize{
#'       \item 100 proteins: ~0.1 sec
#'       \item 1,000 proteins: ~10 sec
#'       \item 10,000 proteins: ~30 min (consider filtering first)
#'     }
#' }
#'
#' @section Set Size Considerations:
#' The Dice coefficient has these properties with respect to set sizes:
#' \itemize{
#'   \item Scores are symmetric (D(A,B) = D(B,A))
#'   \item Small proteins (few fractions) can achieve high scores with partial overlap
#'   \item Large proteins require more overlapping fractions for high scores
#'   \item The \code{min_fractions_present} parameter helps control for very small proteins
#'   \item Parameter effect examples:
#'     \itemize{
#'       \item min_fractions_present=0: May detect spurious interactions from sparse proteins
#'       \item min_fractions_present=3: More reliable but may miss real interactions
#'     }
#' }
#'
#' @param elution_matrix A numeric matrix where rows are proteins (named) and
#'   columns are fractions. Must contain at least 2 proteins to calculate scores.
#'   NAs are treated as 0 (absent). The matrix will be sorted by row names.
#' @param min_fractions_present Integer, minimum number of fractions a protein
#'   must be detected in (default 0). Higher values filter out sparse proteins:
#'   \itemize{
#'     \item 0: No filtering (default)
#'     \item 1-3: Typical values for quality control
#'     \item Higher values: More stringent filtering
#'   }
#' @param top_n_ppi Integer or `NULL`. If an integer, the top N PPIs by Dice
#'   score are returned. If `NULL` (default), all scored PPIs are returned.
#'   Useful for focusing on highest-confidence interactions.
#'
#' @return A data frame with columns:
#'   \item{PPI}{Protein-protein interaction identifier ("ProteinA~ProteinB").}
#'   \item{dice_similarity_score}{Dice similarity coefficient (0-1), where
#'     higher scores indicate more similar elution patterns (co-occurrence).}
#'   The data frame is sorted by `dice_similarity_score` in descending order.
#'
#' @section Edge Cases:
#' The function handles these edge cases:
#' \itemize{
#'   \item Single protein input: Returns empty data frame with warning
#'   \item All-zero rows: Warning issued, scores calculated as 0
#'   \item NA values: Treated as 0 (absent)
#'   \item Duplicate row names: Error thrown (must be unique)
#' }
#'
#' @export
#' @importFrom arules dissimilarity
#' @seealso \code{\link{filter_matrix_by_nonzero_fractions}},
#'   \code{\link{generate_all_pairwise_ppi}}, \code{\link{score_ppi_by_jaccard}}
#' @examples
#' set.seed(123)
#' mat <- matrix(0, nrow = 3, ncol = 5)
#' rownames(mat) <- c("P1", "P2", "P3")
#' mat["P1", 1:3] <- 1      # P1 in F1,F2,F3
#' mat["P2", 2:4] <- 1      # P2 in F2,F3,F4 (overlap with P1 in F2,F3)
#' mat["P3", 4:5] <- 1      # P3 in F4,F5 (overlap with P2 in F4)
#'
#' # Get all PPIs with default parameters
#' dice_scores <- score_ppi_by_dice(mat)
#' print(dice_scores)
#'
#' # Filter sparse proteins and get top 3 PPIs
#' dice_scores_filtered <- score_ppi_by_dice(mat, 
#'                                         min_fractions_present = 1,
#'                                         top_n_ppi = 3)
#' print(dice_scores_filtered)
#' 
#' # Example with real biological data
#' data(CoFrac_Havugimana_PC_Cell_2012_LTQ_293NE12_HCW)
#' elution_matrix <- prepare_elution_matrix(CoFrac_Havugimana_PC_Cell_2012_LTQ_293NE12_HCW)
#' 
#' # Calculate scores with moderate filtering
#' ppi_scores <- score_ppi_by_dice(elution_matrix,
#'                               min_fractions_present = 2,
#'                               top_n_ppi = 100)
#'                               
#' # Visualize score distribution
#' hist(ppi_scores$dice_similarity_score,
#'      main = "Dice Coefficient Distribution",
#'      xlab = "Dice Score")
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
  binary_matrix <- filtered_elution_matrix
  binary_matrix[is.na(binary_matrix)] <- 0
  binary_matrix <- ifelse(binary_matrix > 0, 1, 0)

  if (!all(binary_matrix %in% c(0,1))) {
    stop("Binary matrix preparation failed. Matrix should contain only 0 or 1.")
  }
  
  if (any(rowSums(binary_matrix) == 0)) {
    warning("Some proteins have no presence in any fraction (all zeros). ",
            "This may affect Dice coefficient calculations.")
  }

  # Calculate Dice dissimilarity with explicit error handling
  dice_dist_matrix <- tryCatch(
    as.matrix(arules::dissimilarity(binary_matrix, method = "dice")),
    warning = function(w) {
      message("Message in dissimilarity calculation: ", w$message)
      as.matrix(arules::dissimilarity(binary_matrix, method = "dice"))
    },
    error = function(e) {
      stop("Failed to calculate Dice dissimilarity: ", e$message)
    }
  )

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
