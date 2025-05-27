#' Generate All Possible Pairwise PPIs from a Protein Set
#'
#' Creates a data frame of all unique pairwise protein-protein interactions (PPIs)
#' from a given vector of protein identifiers. Interactions are represented as
#' "ProteinA~ProteinB" where ProteinA is lexicographically smaller than ProteinB.
#'
#' @param protein_vector A character vector of protein identifiers.
#'
#' @return A data frame with columns:
#'   \item{InteractorA}{The first protein in the pair (lexicographically smaller).}
#'   \item{InteractorB}{The second protein in the pair.}
#'   \item{PPI}{A string representing the interaction (e.g., "ProteinA~ProteinB").}
#'   Returns an empty data frame with these columns if fewer than 2 proteins
#'   are provided.
#' @export
#' @importFrom RcppAlgos comboGeneral
#' @examples
#' proteins <- c("ProteinC", "ProteinA", "ProteinB")
#' ppi_df <- generate_all_pairwise_ppi(proteins)
#' print(ppi_df)
#'
#' single_protein <- c("ProteinX")
#' ppi_df_single <- generate_all_pairwise_ppi(single_protein)
#' print(ppi_df_single)
generate_all_pairwise_ppi <- function(protein_vector) {
  unique_proteins <- sort(unique(as.character(protein_vector)))

  if (length(unique_proteins) < 2) {
    return(
      data.frame(
        InteractorA = character(0),
        InteractorB = character(0),
        PPI = character(0),
        stringsAsFactors = FALSE
      )
    )
  }

  # Generate combinations of 2 proteins
  combinations_matrix <- RcppAlgos::comboGeneral(unique_proteins, 2)

  # Ensure InteractorA is always lexicographically smaller than InteractorB
  # RcppAlgos::comboGeneral output is already sorted within rows if input is sorted
  interactor_a <- combinations_matrix[, 1]
  interactor_b <- combinations_matrix[, 2]

  ppi_strings <- paste(interactor_a, interactor_b, sep = "~")

  ppi_data_frame <- data.frame(
    InteractorA = interactor_a,
    InteractorB = interactor_b,
    PPI = ppi_strings,
    stringsAsFactors = FALSE
  )

  return(ppi_data_frame)
}


#' Generate True Positive and True Negative PPIs from Reference Complexes
#'
#' From a list of reference protein complexes, this function derives:
#' 1. True Positive (TP) PPIs: All pairwise interactions occurring within
#'    any of the reference complexes.
#' 2. True Negative (TN) PPIs: All other possible pairwise interactions between
#'    proteins present in the reference complexes that do *not* occur within
#'    any single complex. (This assumes a "closed world" for TNs based on the
#'    provided complexes).
#'
#' @param reference_complex_list A list where each element is a character
#'   vector of protein identifiers representing a known complex.
#'
#' @return A list containing two data frames:
#'   \item{TP}{A data frame of True Positive PPIs with columns `InteractorA`,
#'     `InteractorB`, and `PPI`.}
#'   \item{TN}{A data frame of True Negative PPIs with the same columns.}
#' @export
#' @importFrom RcppAlgos comboGeneral
#' @examples
#' ref_complexes <- list(
#'   cpx1 = c("A", "B", "C"),
#'   cpx2 = c("C", "D")
#' )
#' ppi_sets <- generate_reference_ppi_sets(ref_complexes)
#' print("True Positives (TP):")
#' print(ppi_sets$TP)
#' print("True Negatives (TN):")
#' print(ppi_sets$TN)
generate_reference_ppi_sets <- function(reference_complex_list) {
  if (!is.list(reference_complex_list)) {
    stop("'reference_complex_list' must be a list.")
  }

  # Ensure all complex members are unique within each complex for PPI generation
  cleaned_complex_list <- lapply(reference_complex_list,
                                 function(cpx) unique(as.character(cpx)))

  all_proteins_in_reference <- sort(unique(unlist(cleaned_complex_list)))

  if (length(all_proteins_in_reference) < 2) {
    empty_df <- data.frame(InteractorA = character(0),
                           InteractorB = character(0),
                           PPI = character(0), stringsAsFactors = FALSE)
    return(list(TP = empty_df, TN = empty_df))
  }

  # Generate all possible PPIs from the set of all proteins (potential universe)
  all_possible_ppi_df <-
    generate_all_pairwise_ppi(all_proteins_in_reference)

  # Generate True Positive (TP) PPIs from within-complex interactions
  tp_ppi_list <- lapply(cleaned_complex_list, function(protein_ids_in_cpx) {
    if (length(protein_ids_in_cpx) >= 2) {
      # Sort proteins within complex before generating combinations for consistency
      # generate_all_pairwise_ppi already sorts its input protein_vector
      return(generate_all_pairwise_ppi(protein_ids_in_cpx))
    } else {
      return(NULL) # Return NULL for complexes with < 2 proteins
    }
  })

  # Combine TP PPIs from all complexes and make them unique
  tp_ppi_df <- do.call(rbind, tp_ppi_list)
  if (!is.null(tp_ppi_df) && nrow(tp_ppi_df) > 0) {
    tp_ppi_df <- unique(tp_ppi_df)
    rownames(tp_ppi_df) <- NULL
  } else { # Handle case where no TPs are formed (e.g., all complexes < 2 members)
    tp_ppi_df <- data.frame(InteractorA = character(0),
                            InteractorB = character(0),
                            PPI = character(0), stringsAsFactors = FALSE)
  }


  # True Negative (TN) PPIs are those in all_possible_ppi_df but not in tp_ppi_df
  if (nrow(all_possible_ppi_df) > 0 && nrow(tp_ppi_df) > 0) {
    tn_ppi_df <- all_possible_ppi_df[
      !(all_possible_ppi_df$PPI %in% tp_ppi_df$PPI), ]
  } else if (nrow(all_possible_ppi_df) > 0 && nrow(tp_ppi_df) == 0) {
    # If no TPs, all possible PPIs are TNs (within this context)
    tn_ppi_df <- all_possible_ppi_df
  } else { # No possible PPIs or other edge cases
    tn_ppi_df <- data.frame(InteractorA = character(0),
                            InteractorB = character(0),
                            PPI = character(0), stringsAsFactors = FALSE)
  }
  rownames(tn_ppi_df) <- NULL


  return(list(TP = tp_ppi_df, TN = tn_ppi_df))
}
