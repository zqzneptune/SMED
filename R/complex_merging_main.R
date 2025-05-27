#' Merge Protein Complexes from Two Technique-Specific Lists
#'
#' This is the main function to merge protein complex lists derived from two
#' different experimental techniques or datasets. The process involves:
#' 1. Initial conversion of raw lists to internal S3 objects.
#' 2. Intra-list redundancy removal within each list based on a similarity
#'    threshold.
#' 3. Cross-list merging of similar complexes from the two cleaned lists.
#' 4. Addition of non-merged (technique-specific) complexes.
#' 5. Validation (e.g., size filtering) and ranking of all complexes.
#'
#' @param raw_list1 A list where each element is a character vector of protein
#'   IDs, representing complexes from the first source/technique.
#'   Can be named; if not, default names will be generated.
#' @param raw_list2 A list similar to `raw_list1` for the second
#'   source/technique.
#' @param list1_id Character string, an identifier for `raw_list1` (e.g.,
#'   "AP-MS"). Default is "Technique1".
#' @param list2_id Character string, an identifier for `raw_list2` (e.g.,
#'   "Co-Frac"). Default is "Technique2".
#' @param list1_quality_score Numeric value between 0 and 1, representing the
#'   overall quality or confidence in `raw_list1` (1 is best). Default is 1.0.
#' @param list2_quality_score Numeric value (0-1) for `raw_list2`.
#'   Default is 1.0.
#' @param redundancy_threshold Numeric (0-1), similarity threshold for
#'   removing redundant complexes *within* each list. Default is 0.9.
#' @param similarity_method_intra Character, method for intra-list similarity.
#'   Options: "jaccard" (default), "simpson", "dice".
#' @param merge_similarity_threshold Numeric (0-1), similarity threshold for
#'   merging complexes *between* the two lists. Default is 0.7.
#' @param similarity_method_inter Character, method for inter-list similarity.
#'   Options: "simpson" (default), "dice", "jaccard".
#' @param validation_min_size Integer, minimum number of proteins for a complex
#'   to be retained after merging/validation. Default is 2.
#' @param validation_max_size Integer, maximum number of proteins. Default is 50.
#' @param validation_bonus_merged Numeric, a multiplicative bonus applied to the
#'   score of complexes supported by both techniques. Default 1.5.
#' @param verbose Logical, if `TRUE`, progress messages will be printed.
#'   Default `TRUE`.
#'
#' @return A list of `ProteinComplex` S3 objects (see
#'   \code{\link{ProteinComplex-S3}} for structure). This list contains all
#'   complexes (merged and technique-specific) that passed validation,
#'   ranked by their final score.
#'
#' @export
#' @seealso \code{\link{get_merged_complexes}},
#'   \code{\link{get_technique_specific_complexes}},
#'   \code{\link{get_all_complex_summary}},
#'   \code{\link{extract_all_protein_lists}}
#' @examples
#' set.seed(123)
#' list1_data <- list(
#'   CpxA_t1 = c("P1", "P2", "P3", "P4"),
#'   CpxB_t1 = c("P3", "P4", "P5", "P6"),
#'   CpxOnly1 = c("P10", "P11")
#' )
#' list2_data <- list(
#'   CpxX_t2 = c("P1", "P2", "P3", "P5"), # Shares with CpxA_t1
#'   CpxY_t2 = c("P3", "P4", "P5", "P6", "P7", "P8"), # Shares with CpxB_t1
#'   CpxOnly2 = c("P20", "P21")
#' )
#'
#' results <- merge_protein_complexes(
#'   raw_list1 = list1_data, raw_list2 = list2_data,
#'   list1_id = "APMS", list2_id = "CoFrac",
#'   list1_quality_score = 0.9, list2_quality_score = 0.8,
#'   merge_similarity_threshold = 0.5, # Lowered for more merges in example
#'   similarity_method_inter = "simpson",
#'   verbose = FALSE
#' )
#' cat("Total complexes in results:", length(results), "\n")
#' # Explore results using extractor functions (see their examples)
merge_protein_complexes <- function(raw_list1, raw_list2,
                                    list1_id = "Technique1",
                                    list2_id = "Technique2",
                                    list1_quality_score = 1.0,
                                    list2_quality_score = 1.0,
                                    redundancy_threshold = 0.9,
                                    similarity_method_intra = "jaccard",
                                    merge_similarity_threshold = 0.7,
                                    similarity_method_inter = "simpson",
                                    validation_min_size = 2,
                                    validation_max_size = 50,
                                    validation_bonus_merged = 1.5,
                                    verbose = TRUE) {

  # --- Input Validation ---
  if (list1_quality_score < 0 || list1_quality_score > 1) {
    stop("'list1_quality_score' must be between 0 and 1.")
  }
  if (list2_quality_score < 0 || list2_quality_score > 1) {
    stop("'list2_quality_score' must be between 0 and 1.")
  }
  valid_sim_methods <- c("simpson", "dice", "jaccard")
  if (!similarity_method_intra %in% valid_sim_methods) {
    stop("Invalid 'similarity_method_intra'. Choose from: ",
         paste(valid_sim_methods, collapse = ", "))
  }
  if (!similarity_method_inter %in% valid_sim_methods) {
    stop("Invalid 'similarity_method_inter'. Choose from: ",
         paste(valid_sim_methods, collapse = ", "))
  }
  if (!is.list(raw_list1) || !is.list(raw_list2)){
    stop("'raw_list1' and 'raw_list2' must be lists.")
  }


  if (verbose) message("Starting protein complex merging process...")

  # --- Preparation and Intra-List Cleaning ---
  raw_list1_named <- ensure_named_list(raw_list1, list1_id)
  raw_list2_named <- ensure_named_list(raw_list2, list2_id)

  s3_list1 <- convert_raw_list_to_s3(raw_list1_named, list1_id,
                                     list1_quality_score)
  s3_list2 <- convert_raw_list_to_s3(raw_list2_named, list2_id,
                                     list2_quality_score)

  if (verbose) {
    message("List 1 ('", list1_id, "') initial complexes: ", length(s3_list1))
    message("List 2 ('", list2_id, "') initial complexes: ", length(s3_list2))
  }

  if (verbose) message("Cleaning list 1 (method: ",
                       similarity_method_intra, ")...")
  cleaned_list1 <- remove_redundancy_intra_list(s3_list1,
                                                redundancy_threshold,
                                                similarity_method_intra)
  if (verbose) message("Cleaned list 1 size: ", length(cleaned_list1))

  if (verbose) message("Cleaning list 2 (method: ",
                       similarity_method_intra, ")...")
  cleaned_list2 <- remove_redundancy_intra_list(s3_list2,
                                                redundancy_threshold,
                                                similarity_method_intra)
  if (verbose) message("Cleaned list 2 size: ", length(cleaned_list2))

  # --- Inter-List Merging ---
  # Stores all complexes: merged ones, list1-only, list2-only
  all_processed_complexes <- list()
  # Keep track of which complexes from list2 have been merged
  used_indices_list2 <- rep(FALSE, length(cleaned_list2))

  if (verbose) message("Starting cross-list comparison (method: ",
                       similarity_method_inter, ") and merging...")

  if (length(cleaned_list1) > 0) {
    for (i in seq_along(cleaned_list1)) {
      complex1 <- cleaned_list1[[i]]
      best_match_complex2 <- NULL
      best_similarity_score <- -1 # Start below any valid threshold
      best_match_idx2 <- -1

      if (length(cleaned_list2) > 0) {
        for (j in seq_along(cleaned_list2)) {
          if (!used_indices_list2[j]) { # Only consider unused list2 complexes
            complex2 <- cleaned_list2[[j]]
            similarity <- calculate_similarity(complex1, complex2,
                                               method = similarity_method_inter)

            if (similarity >= merge_similarity_threshold &&
                similarity > best_similarity_score) {
              best_similarity_score <- similarity
              best_match_complex2 <- complex2
              best_match_idx2 <- j
            }
          }
        }
      }

      if (!is.null(best_match_complex2) && best_match_idx2 != -1) {
        if (verbose) {
          message(
            "  Merging: '", complex1$original_name, "' (", list1_id,
            ") with '", best_match_complex2$original_name, "' (", list2_id,
            ") - Sim (", similarity_method_inter, "): ",
            round(best_similarity_score, 3)
          )
        }
        merged_c <- merge_two_complexes(complex1,
                                        best_match_complex2,
                                        best_similarity_score)
        all_processed_complexes <- append(all_processed_complexes,
                                          list(merged_c))
        used_indices_list2[best_match_idx2] <- TRUE
      } else {
        # Complex1 did not find a merge partner, add as technique-specific
        complex1$support_level <- paste0(complex1$source_list_id, "_only")
        all_processed_complexes <- append(all_processed_complexes,
                                          list(complex1))
      }
    }
  }

  # Add remaining (unmerged) complexes from list2
  if (verbose && length(cleaned_list2) > 0 &&
      sum(!used_indices_list2) > 0) {
    message("Adding ", sum(!used_indices_list2),
            " remaining specific complexes from list 2 ('", list2_id, "')...")
  }
  if (length(cleaned_list2) > 0) {
    for (j in seq_along(cleaned_list2)) {
      if (!used_indices_list2[j]) {
        complex2 <- cleaned_list2[[j]]
        complex2$support_level <- paste0(complex2$source_list_id, "_only")
        all_processed_complexes <- append(all_processed_complexes,
                                          list(complex2))
      }
    }
  }

  # --- Final Validation and Ranking ---
  if (verbose) {
    message("Validating and ranking ", length(all_processed_complexes),
            " total complexes (merged and specific)...")
  }
  validated_ranked_complexes <- validate_and_rank_complexes(
    all_processed_complexes,
    min_size = validation_min_size,
    max_size = validation_max_size,
    both_techniques_bonus = validation_bonus_merged
  )

  if (verbose) {
    message("Final number of validated and ranked complexes: ",
            length(validated_ranked_complexes))
    message("Complex merging process complete.")
  }

  return(validated_ranked_complexes)
}
