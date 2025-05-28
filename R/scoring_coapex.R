#' Score PPIs based on Co-Apex of Elution Profiles
#'
#' Calculates a "co-apex" score for potential protein-protein interactions (PPIs)
#' based on the proximity of their peak elution fractions in a chromatography
#' experiment. Proteins with closer apex fractions receive higher scores.
#'
#' @details
#' The co-apex scoring algorithm works as follows:
#' 1. For each protein, identify the fraction(s) with maximum intensity (apex)
#' 2. If a protein has two maxima within `max_apex_spread` fractions, average them
#' 3. Calculate absolute differences in apex fractions for all protein pairs
#' 4. Normalize differences to 0-1 scale where:
#'    - 1 = proteins share identical apex fraction(s)
#'    - 0 = proteins have maximum possible apex difference
#' 
#' Mathematical formulation:
#' For proteins A and B with apex fractions f_A and f_B:
#' raw_score = |f_A - f_B|
#' normalized_score = (max_diff - raw_score) / (max_diff - min_diff)
#' where max_diff and min_diff are the maximum and minimum differences observed
#' across all protein pairs in the dataset.
#'
#' Performance characteristics:
#' - Time complexity: O(n^2) where n is number of proteins (due to pairwise comparisons)
#' - Memory usage: Scales with number of PPIs generated (n*(n-1)/2 pairs)
#' - Handles edge cases:
#'   - Proteins with identical apexes (score = 1)
#'   - Proteins with no detectable apex (excluded)
#'   - All proteins having same apex difference (special normalization)
#'
#' Biological interpretation:
#' Proteins that co-elute in the same fractions are more likely to be part of
#' the same protein complex. This scoring method quantifies that likelihood
#' based on chromatographic co-elution patterns.
#'
#' @param elution_matrix A numeric matrix where rows are proteins (named) and
#'   columns are fractions. Must contain at least 2 proteins to calculate scores.
#'   Missing values (NA) are treated as 0 intensity for apex calculation.
#' @param min_fractions_present Integer, minimum number of fractions a protein
#'   must be detected in to be considered. Passed to
#'   `filter_matrix_by_nonzero_fractions`. Default is 0 (no pre-filtering by
#'   this function, but `filter_matrix_by_nonzero_fractions` default is 2).
#'   It is advisable to use `filter_matrix_by_nonzero_fractions` prior to this.
#'   This argument here provides a convenience filter.
#' @param max_apex_spread Integer, the maximum allowable spread (difference in
#'   fraction number) if a protein has two maximal fractions for them to be
#'   averaged into a single apex. Default is 2. If spread is larger, or more
#'   than two maxima, the protein is excluded from apex calculation.
#' @param top_n_ppi Integer or `NULL`. If an integer, the top N PPIs by score
#'   are returned. If `NULL` (default), all scored PPIs are returned.
#'
#' @return A data frame with columns:
#'   \item{PPI}{Protein-protein interaction identifier ("ProteinA~ProteinB").}
#'   \item{norm_coapex_score}{Normalized co-apex score (0-1 scale), where
#'     1 = identical apex fractions, 0 = maximum observed difference.
#'     Higher scores indicate closer apexes.}
#'   The data frame is sorted by `norm_coapex_score` in descending order.
#' @export
#' @seealso \code{\link{filter_matrix_by_nonzero_fractions}},
#'   \code{\link{generate_all_pairwise_ppi}}
#' @examples
#' # Create a dummy elution matrix
#' set.seed(123)
#' mat <- matrix(0, nrow = 4, ncol = 10)
#' rownames(mat) <- c("P1", "P2", "P3", "P4")
#' colnames(mat) <- paste0("F", 1:10)
#' mat["P1", 3:4] <- c(5,5); mat["P2", 4] <- 6 # P1, P2 co-apex
#' mat["P3", 8] <- 7; mat["P4", 2] <- 4      # P3, P4 distant
#' mat["P1",1] <- 1; mat["P2",1] <- 1; mat["P3",1] <- 1; mat["P4",1] <- 1 # min_fractions
#'
#' # Calculate co-apex scores
#' coapex_scores <- score_ppi_by_coapex(mat, min_fractions_present = 1, top_n_ppi = 5)
#' print(coapex_scores)
#' 
#' # Interpretation of example results:
#' # P1~P2 has highest score (1.0) since they share apex at fraction 4
#' # P3~P4 has lowest score since their apexes are farthest apart (fractions 8 vs 2)
score_ppi_by_coapex <- function(elution_matrix,
                              min_fractions_present = 0,
                              max_apex_spread = 2,
                              top_n_ppi = NULL) {

  if (!is.matrix(elution_matrix) || !is.numeric(elution_matrix)) {
    stop("'elution_matrix' must be a numeric matrix.")
  }
  if (is.null(rownames(elution_matrix))) {
    stop("'elution_matrix' must have row names (protein identifiers).")
  }

  # Filter matrix based on presence in min_fractions_present
  # Note: filter_matrix_by_nonzero_fractions default for min_fractions is 2.
  # If min_fractions_present = 0 passed here, it will use that.
  filtered_elution_matrix <- filter_matrix_by_nonzero_fractions(
    elution_matrix,
    min_fractions = min_fractions_present
  )

  if (nrow(filtered_elution_matrix) < 2) {
    message("Less than 2 proteins after filtering, cannot calculate co-apex scores.")
    return(data.frame(PPI = character(0),
                      norm_coapex_score = numeric(0)))
  }

  # Impute NAs with 0 for apex calculation
  filtered_elution_matrix[is.na(filtered_elution_matrix)] <- 0

  # Determine apex fraction for each protein
  protein_apexes <- apply(filtered_elution_matrix, 1, function(protein_row) {
    max_val <- max(protein_row, na.rm = TRUE)
    if (max_val == 0 && all(protein_row == 0)) return(NA_real_) # All zeros

    max_pos_indices <- which(protein_row == max_val)

    if (length(max_pos_indices) == 1) {
      return(as.numeric(max_pos_indices[1]))
    } else if (length(max_pos_indices) == 2) {
      if ((max_pos_indices[2] - max_pos_indices[1]) <= max_apex_spread) {
        return(mean(max_pos_indices))
      } else {
        return(NA_real_) # Spread too large
      }
    } else {
      return(NA_real_) # More than two maxima or other ambiguous cases
    }
  })

  # Filter out proteins where apex could not be determined
  valid_apexes <- protein_apexes[!is.na(protein_apexes)]
  protein_names_with_valid_apexes <- names(valid_apexes)

  if (length(valid_apexes) < 2) {
    message("Less than 2 proteins with valid apexes, co-apex scores not possible.")
    return(data.frame(PPI = character(0),
                      norm_coapex_score = numeric(0)))
  }

  # Generate all pairwise PPIs for proteins with valid apexes
  pairwise_ppi_df <-
    generate_all_pairwise_ppi(protein_names_with_valid_apexes)

  if (nrow(pairwise_ppi_df) == 0) {
    return(data.frame(PPI = character(0),
                      norm_coapex_score = numeric(0)))
  }

  # Calculate absolute differences in apex fractions for these PPIs
  apex_diffs <- abs(valid_apexes[pairwise_ppi_df$InteractorA] -
                    valid_apexes[pairwise_ppi_df$InteractorB])

  # Normalize co-apex scores (0-1, higher is better)
  # Score = (max_diff - diff) / (max_diff - min_diff)
  # If all diffs are same (max_diff == min_diff), this can lead to NaN/Inf
  min_apex_diff <- min(apex_diffs, na.rm = TRUE)
  max_apex_diff <- max(apex_diffs, na.rm = TRUE)

  if (max_apex_diff == min_apex_diff) {
    # All differences are the same. If diff is 0, all perfectly co-apex, score 1.
    # Otherwise, if all diffs are non-zero but same, score could be 0 or 0.5.
    # Let's assign 1 if the common difference is 0, and 0.5 otherwise.
    # This is somewhat arbitrary; ideally, such cases reflect perfect correlation or none.
    # A score of 1 for zero difference, 0 for non-zero difference is also an option.
    norm_scores <- ifelse(max_apex_diff == 0, 1, 0.5)
  } else {
    norm_scores <- (max_apex_diff - apex_diffs) /
      (max_apex_diff - min_apex_diff)
  }

  pairwise_ppi_df$norm_coapex_score <- norm_scores

  # Sort by score (descending)
  final_ppi_df <- pairwise_ppi_df[
    order(pairwise_ppi_df$norm_coapex_score, decreasing = TRUE),
  ]
  rownames(final_ppi_df) <- NULL

  # Select top N PPIs if requested
  if (!is.null(top_n_ppi) && nrow(final_ppi_df) > top_n_ppi) {
    final_ppi_df <- final_ppi_df[seq_len(top_n_ppi), ]
  }

  return(final_ppi_df[, c("PPI", "norm_coapex_score")])
}
