#' Generate All Possible Pairwise PPIs from a Protein Set
#'
#' Creates a comprehensive data frame of all unique pairwise protein-protein 
#' interactions (PPIs) from a given vector of protein identifiers using 
#' combinatorial generation. This serves as the foundation for many PPI analysis
#' workflows.
#'
#' @details
#' Algorithm:
#' 1. Input proteins are deduplicated and sorted lexicographically
#' 2. All unique pairwise combinations (n choose 2) are generated
#' 3. Each interaction is represented as "ProteinA~ProteinB" where ProteinA is 
#'    lexicographically smaller than ProteinB to ensure consistent representation
#'    of bidirectional interactions
#'
#' Biological Considerations:
#' - Assumes all input proteins are from the same organism/species
#' - No filtering is applied - all possible pairs are generated regardless of
#'   biological plausibility
#' - Protein identifiers should be consistent (e.g., all UniProt IDs or all gene symbols)
#'
#' @param protein_vector A character vector of protein identifiers. Must be 
#'   non-missing and coercible to character. Duplicates are automatically removed.
#'
#' @return A data frame with columns:
#'   \item{InteractorA}{The first protein in the pair (lexicographically smaller)}
#'   \item{InteractorB}{The second protein in the pair}
#'   \item{PPI}{A string representing the interaction (e.g., "ProteinA~ProteinB")}
#'   Returns an empty data frame with these columns if fewer than 2 proteins
#'   are provided.
#'
#' @export
#' @importFrom RcppAlgos comboGeneral
#' @seealso \code{\link{generate_reference_ppi_sets}} for generating TP/TN PPIs,
#'   \code{\link{generate_all_pairwise_ppi}} (man page) for additional examples
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
#' Derives gold-standard interaction sets from known complexes by:
#' 1. True Positives (TP): All pairwise interactions within complexes
#' 2. True Negatives (TN): All other possible interactions between complex members
#'
#' @details
#' Algorithm:
#' 1. Collect all unique proteins from reference complexes
#' 2. Generate all possible pairwise interactions (universe)
#' 3. Extract TP PPIs as within-complex interactions
#' 4. Define TN PPIs as universe minus TP PPIs
#'
#' Biological Considerations:
#' - TP definition assumes all within-complex interactions are true
#' - TN definition assumes absence from complexes implies non-interaction
#'   (strong "closed world" assumption)
#' - Complex quality directly impacts result reliability
#' - Protein identifiers must be consistent across complexes
#'
#' @param reference_complex_list A list where each element is a character
#'   vector of protein identifiers representing a known complex. Each complex
#'   should contain at least 2 proteins to generate meaningful PPIs.
#'
#' @return A list containing two data frames:
#'   \item{TP}{True Positive PPIs (within-complex interactions)}
#'   \item{TN}{True Negative PPIs (between but not within complexes)}
#'   Both have columns `InteractorA`, `InteractorB`, and `PPI`
#'
#' @export
#' @importFrom RcppAlgos comboGeneral
#' @seealso \code{\link{generate_all_pairwise_ppi}} for base PPI generation,
#'   \code{\link{generate_reference_ppi_sets}} (man page) for more examples
#' @examples
#' ref_complexes <- list(
#'   cpx1 = c("A", "B", "C"), # A complex with 3 proteins
#'   cpx2 = c("C", "D")       # A binary interaction
#' )
#' ppi_sets <- generate_reference_ppi_sets(ref_complexes)
#' print("True Positives (TP):")
#' print(ppi_sets$TP)  # A-B, A-C, B-C from cpx1; C-D from cpx2
#' print("True Negatives (TN):")
#' print(ppi_sets$TN)  # All other possible pairs (A-D, B-D)
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
