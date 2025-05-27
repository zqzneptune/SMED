#' Generate Reference Protein-Protein Interactions (PPIs)
#'
#' This function processes a list of reference protein complexes to generate
#' "True Positive" (TP) and "True Negative" (TN) PPIs. TP PPIs are pairs
#' of proteins found within the same reference complex. TN PPIs are all other
#' possible pairs of proteins (from the set of all proteins in the reference)
#' that are not observed as TPs.
#'
#' @param reference_complexes A list where each element is a character vector
#'   representing the members (e.g., gene or protein IDs) of a protein complex.
#'   Members named "None" will be ignored.
#'
#' @return A list containing two data frames:
#'   \item{TP}{A data frame of True Positive PPIs with columns:
#'     `InteractorA`, `InteractorB`, and `PPI` (e.g., "Protein1~Protein2").
#'     Interactors in a pair are alphabetically sorted.}
#'   \item{TN}{A data frame of True Negative PPIs, with the same columns
#'     as `TP`.}
#'
#' @details
#' The function first identifies all unique proteins across all provided
#' complexes. It then generates all possible unique pairs of these proteins,
#' forming the universe of potential PPIs.
#'
#' For TP PPIs: For each complex, if it contains two or more members (after
#' removing duplicates and "None" placeholders), all pairwise combinations of
#' its members are generated. These pairs are formatted as "InteractorA~InteractorB",
#' ensuring InteractorA is alphabetically before InteractorB.
#'
#' For TN PPIs: These are all pairs from the universe of potential PPIs that
#' were not identified as TP PPIs.
#'
#' The special string "None" is filtered out from complex members.
#'
#' @examples
#' # Dummy reference complexes for example
#' # In a real scenario, this might come from a database or experimental data.
#' # For SMED package, load_reference_corum_havugimana() could be used.
#' complex1 <- c("GeneA", "GeneB", "GeneC")
#' complex2 <- c("GeneB", "GeneD", "None") # "None" will be ignored
#' complex3 <- c("GeneE") # Will not generate TPs as it has < 2 members
#' complex4 <- c("GeneC", "GeneA") # Duplicate of pairs from complex1
#'
#' example_ref_cpx <- list(complex1, complex2, complex3, complex4)
#'
#' # Generate TP and TN PPIs
#' # Ensure RcppAlgos is installed: install.packages("RcppAlgos")
#' if (requireNamespace("RcppAlgos", quietly = TRUE)) {
#'   ref_ppi_sets <- generate_refppi(example_ref_cpx)
#'   print("True Positives (TP):")
#'   print(head(ref_ppi_sets$TP))
#'   print("True Negatives (TN):")
#'   print(head(ref_ppi_sets$TN))
#' }
#'
#' \donttest{
#' # Example using a hypothetical loader from the SMED package (if available)
#' # Assuming load_reference_corum_havugimana() is part of SMED
#' # and returns a list of character vectors.
#' # ref_cpx_data <- SMED::load_reference_corum_havugimana()
#' # if (length(ref_cpx_data) > 10) {
#' #   ref_ppi_subset <- generate_refppi(ref_cpx_data[1:10])
#' #   summary(ref_ppi_subset$TP)
#' # }
#' }
#'
#' @importFrom RcppAlgos comboGeneral
#' @importFrom utils combn
#' @export
generate_refppi <- function(reference_complexes) {

  # --- Input Validation and Basic Checks ---
  if (!is.list(reference_complexes)) {
    stop("'reference_complexes' must be a list of character vectors.")
  }
  if (length(reference_complexes) == 0) {
    message("Input 'reference_complexes' is empty. Returning empty PPI sets.")
    empty_df <- ._create_ppi_dataframe_from_strings(character(0))
    return(list(TP = empty_df, TN = empty_df))
  }
  if (!all(vapply(reference_complexes, is.character, logical(1)))) {
    stop("All elements in 'reference_complexes' list must be character vectors.")
  }

  # --- Prepare Gene/Protein Universe ---
  all_genes_flat <- unlist(reference_complexes, use.names = FALSE)
  all_genes_flat <- all_genes_flat[all_genes_flat != "None" &
                                     !is.na(all_genes_flat) &
                                     nzchar(all_genes_flat)]

  if (length(all_genes_flat) == 0) {
    message("No valid gene/protein names found after filtering. Returning empty PPI sets.")
    empty_df <- ._create_ppi_dataframe_from_strings(character(0))
    return(list(TP = empty_df, TN = empty_df))
  }

  unique_sorted_genes <- sort(unique(all_genes_flat))

  if (length(unique_sorted_genes) < 2) {
    message("Fewer than two unique genes found. Cannot form PPIs.")
    empty_df <- ._create_ppi_dataframe_from_strings(character(0))
    return(list(TP = empty_df, TN = empty_df))
  }

  # --- Generate All Potential PPIs (Universe) ---
  # RcppAlgos::comboGeneral on sorted input ensures pairs are (gene1, gene2)
  # where gene1 < gene2.
  all_pairs_matrix <- RcppAlgos::comboGeneral(unique_sorted_genes, 2)
  all_potential_ppi_strings <- paste(
    all_pairs_matrix[, 1], all_pairs_matrix[, 2], sep = "~"
  )

  # --- Generate True Positive (TP) PPIs ---
  tp_ppi_list <- lapply(reference_complexes, function(current_complex) {
    # Clean members: unique, not "None", valid characters
    complex_members <- unique(current_complex)
    complex_members <- complex_members[complex_members != "None" &
                                         !is.na(complex_members) &
                                         nzchar(complex_members)]

    if (length(complex_members) >= 2) {
      # utils::combn with FUN=sort ensures pairs are ("GeneA", "GeneB")
      # where GeneA is alphabetically smaller than GeneB.
      member_pairs_matrix <- utils::combn(complex_members, 2, FUN = sort)

      # If only one pair, combn returns a vector.
      # Convert to matrix to handle consistently.
      if (is.vector(member_pairs_matrix)) {
        member_pairs_matrix <- matrix(member_pairs_matrix, nrow = 2)
      }

      return(paste(member_pairs_matrix[1, ],
                   member_pairs_matrix[2, ], sep = "~"))
    } else {
      return(NULL) # No pairs if fewer than 2 members
    }
  })

  true_positive_ppi_strings <- unique(unlist(tp_ppi_list, use.names = FALSE))
  # Filter out any NULLs that became NAs or actual NAs if any
  true_positive_ppi_strings <- true_positive_ppi_strings[
    !is.na(true_positive_ppi_strings)
  ]

  true_positive_ppi_df <-
    ._create_ppi_dataframe_from_strings(true_positive_ppi_strings)

  # --- Generate True Negative (TN) PPIs ---
  # TNs are those in all_potential_ppi_strings but not in TP set.
  true_negative_ppi_strings <- setdiff(
    all_potential_ppi_strings,
    true_positive_ppi_strings
  )

  true_negative_ppi_df <-
    ._create_ppi_dataframe_from_strings(true_negative_ppi_strings)

  return(list(TP = true_positive_ppi_df, TN = true_negative_ppi_df))
}


#' Internal Helper to Create PPI DataFrame
#'
#' Converts a character vector of PPI strings (e.g., "ProteinA~ProteinB")
#' into a standardized data frame.
#'
#' @param ppi_strings_vector A character vector of PPIs.
#' @return A data frame with columns InteractorA, InteractorB, PPI.
#' @noRd
._create_ppi_dataframe_from_strings <- function(ppi_strings_vector) {
  if (length(ppi_strings_vector) == 0) {
    return(
      data.frame(
        InteractorA = character(0L),
        InteractorB = character(0L),
        PPI = character(0L),
        stringsAsFactors = FALSE
      )
    )
  }

  # Splitting "InteractorA~InteractorB" strings
  # Assumes valid format with exactly one "~" separator per string.
  # Also assumes interactors within the string are already sorted alphabetically.
  split_interactions <- strsplit(ppi_strings_vector, "~", fixed = TRUE)

  # Check for malformed strings (not exactly two parts after split)
  # This is a basic check; more complex validation could be added if needed.
  valid_splits <- vapply(split_interactions, length, integer(1)) == 2L
  if (!all(valid_splits)) {
    warning(
      "Some PPI strings did not conform to 'InteractorA~InteractorB' format ",
      "and were excluded."
    )
    split_interactions <- split_interactions[valid_splits]
    ppi_strings_vector <- ppi_strings_vector[valid_splits]

    # If all strings were malformed
    if (length(ppi_strings_vector) == 0) {
      return(
        data.frame(
          InteractorA = character(0L),
          InteractorB = character(0L),
          PPI = character(0L),
          stringsAsFactors = FALSE
        )
      )
    }
  }

  interactor_matrix <- do.call(rbind, split_interactions)

  result_df <- data.frame(
    InteractorA = interactor_matrix[, 1],
    InteractorB = interactor_matrix[, 2],
    PPI = ppi_strings_vector,
    stringsAsFactors = FALSE
  )
  return(result_df)
}
