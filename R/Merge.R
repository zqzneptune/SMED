# Complex R/complex_merging.R

#' Merge Protein Complexes from Two Lists
#'
#' @description
#' This package provides functions to merge protein complex lists obtained from
#' two different experimental techniques or datasets. It removes redundancy
#' within each list, then merges complexes across lists based on protein
#' similarity (Simpson or Dice coefficient), and finally validates and ranks
#' the resulting unified set. The quality of the input lists can be specified
#' to influence the stability score of merged complexes. Additional functions
#' are provided to extract subsets of complexes from the results.
#'
#' @docType package
#' @name complexmerger
NULL

# --- S3 Class Definition for ProteinComplex ---

#' ProteinComplex S3 Object Structure
#' @name ProteinComplex
#' @keywords internal
NULL # Documentation provided in previous response, retained for context


#' Create a New ProteinComplex S3 Object
#' @keywords internal
#' @noRd
new_protein_complex <- function(proteins,
                                list_quality_score_origin = NA_real_,
                                original_name = NA_character_,
                                source_list_id = NA_character_,
                                metadata = list()) {

  proteins_char <- as.character(proteins)
  processed_proteins <- sort(unique(proteins_char))

  obj <- list(
    proteins = processed_proteins,
    list_quality_score_origin = as.numeric(list_quality_score_origin),
    original_name = as.character(original_name),
    source_list_id = as.character(source_list_id),

    support_level = NA_character_,
    merged_from_similarity = NA_real_,
    merged_stability_score = NA_real_,
    final_score = NA_real_,

    technique1_data = NULL,
    technique2_data = NULL,

    core_proteins = character(0),
    technique1_specific_proteins = character(0),
    technique2_specific_proteins = character(0),

    original_metadata = metadata
  )
  class(obj) <- "ProteinComplex"
  return(obj)
}

#' Convert Raw List to List of ProteinComplex S3 Objects
#' @keywords internal
#' @noRd
convert_raw_list_to_s3 <- function(raw_list, list_id_name,
                                   list_quality_score) {
  s3_list <- lapply(names(raw_list), function(name) {
    proteins_vector <- raw_list[[name]]
    new_protein_complex(
      proteins = proteins_vector,
      list_quality_score_origin = list_quality_score,
      original_name = name,
      source_list_id = list_id_name
    )
  })
  names(s3_list) <- names(raw_list)
  return(s3_list)
}

#' Ensure Input List is Named
#' @keywords internal
#' @noRd
ensure_named_list <- function(complex_list, list_id_prefix) {
  if (!is.list(complex_list)) {
    stop("Input must be a list.")
  }
  current_names <- names(complex_list)
  n_complexes <- length(complex_list)

  has_names <- !is.null(current_names)
  names_complete <- has_names && length(current_names) == n_complexes
  names_valid <- names_complete && !any(current_names == "")
  names_unique <- names_valid && !any(duplicated(current_names))

  if (!names_unique) {
    message(
      "Input list for '", list_id_prefix,
      "' is unnamed or has problematic names. Assigning default names."
    )
    default_names <- paste0(list_id_prefix, "_Complex_", seq_len(n_complexes))
    names(complex_list) <- default_names
  }
  return(complex_list)
}

# --- Similarity Calculation Functions ---

#' Calculate Similarity Between Two Complexes using Specified Method
#' @keywords internal
#' @noRd
calculate_similarity <- function(complex1, complex2,
                                 method = "simpson") {
  if (!inherits(complex1, "ProteinComplex") ||
      !inherits(complex2, "ProteinComplex")) {
    stop("Inputs must be ProteinComplex objects.")
  }
  proteins1 <- complex1$proteins
  proteins2 <- complex2$proteins

  len1 <- length(proteins1)
  len2 <- length(proteins2)

  if (len1 == 0 && len2 == 0) return(1.0)
  if (len1 == 0 || len2 == 0) return(0.0)

  intersection_set <- intersect(proteins1, proteins2)
  len_intersect <- length(intersection_set)

  if (method == "simpson") {
    min_len <- min(len1, len2)
    if (min_len == 0) return(0.0)
    return(len_intersect / min_len)
  } else if (method == "dice") {
    sum_len <- len1 + len2
    if (sum_len == 0) return(0.0)
    return((2 * len_intersect) / sum_len)
  } else if (method == "jaccard") {
    union_set <- union(proteins1, proteins2)
    if (length(union_set) == 0) return(0.0)
    return(len_intersect / length(union_set))
  }
  else {
    stop("Invalid similarity calculation method specified: ", method,
         ". Choose 'simpson', 'dice', or 'jaccard'.")
  }
}

# --- Intra-List Redundancy Removal ---

#' Remove Redundant Complexes Within a Single List
#' @keywords internal
#' @noRd
remove_redundancy <- function(complex_s3_list, redundancy_threshold = 0.9,
                              similarity_method_intra = "jaccard") {
  if (length(complex_s3_list) == 0) return(list())

  sizes <- sapply(complex_s3_list, function(c) length(c$proteins))
  order_indices <- order(sizes, decreasing = TRUE)
  sorted_complexes <- complex_s3_list[order_indices]

  final_cleaned_list <- list()

  for (i in seq_along(sorted_complexes)) {
    current_complex <- sorted_complexes[[i]]
    is_redundant_with_kept <- FALSE

    if (length(final_cleaned_list) > 0) {
      for (kept_complex in final_cleaned_list) {
        similarity <- calculate_similarity(current_complex, kept_complex,
                                           method = similarity_method_intra)

        if (similarity >= redundancy_threshold) {
          is_redundant_with_kept <- TRUE
          break
        }
      }
    }

    if (!is_redundant_with_kept) {
      final_cleaned_list <- append(final_cleaned_list, list(current_complex))
    }
  }
  return(final_cleaned_list)
}

# --- Merging Two Complexes ---

#' Merge Two Similar ProteinComplex Objects
#' @keywords internal
#' @noRd
merge_two_complexes_internal <- function(complex1, complex2,
                                         similarity_score) {

  merged_proteins <- sort(unique(union(complex1$proteins, complex2$proteins)))

  c1_name_short <- substr(complex1$original_name, 1, 20)
  c2_name_short <- substr(complex2$original_name, 1, 20)
  merged_original_name <- paste("MERGED", c1_name_short, c2_name_short,
                                sep = "_")

  q1 <- complex1$list_quality_score_origin
  q2 <- complex2$list_quality_score_origin
  calculated_merged_stability <- (q1 + q2) / 2

  merged_obj <- new_protein_complex(
    proteins = merged_proteins,
    list_quality_score_origin = calculated_merged_stability,
    original_name = merged_original_name,
    source_list_id = "merged"
  )

  merged_obj$support_level <- "both_techniques"
  merged_obj$merged_from_similarity <- similarity_score
  merged_obj$merged_stability_score <- calculated_merged_stability

  merged_obj$technique1_data <- complex1
  merged_obj$technique2_data <- complex2

  merged_obj$core_proteins <-
    sort(unique(intersect(complex1$proteins, complex2$proteins)))
  merged_obj$technique1_specific_proteins <-
    sort(unique(setdiff(complex1$proteins, complex2$proteins)))
  merged_obj$technique2_specific_proteins <-
    sort(unique(setdiff(complex2$proteins, complex1$proteins)))

  return(merged_obj)
}

# --- Final Validation and Ranking ---

#' Validate and Rank Merged Complexes
#' @keywords internal
#' @noRd
validate_and_rank_complexes <- function(merged_complexes_list,
                                        min_size = 2,
                                        max_size = 50,
                                        both_techniques_bonus = 1.5) {
  validated_complexes <- list()

  for (complex_obj in merged_complexes_list) {
    num_proteins <- length(complex_obj$proteins)
    if (num_proteins >= min_size && num_proteins <= max_size) {

      if (identical(complex_obj$support_level, "both_techniques")) {
        complex_obj$final_score <-
          complex_obj$merged_stability_score * both_techniques_bonus
      } else {
        complex_obj$final_score <- complex_obj$list_quality_score_origin
      }
      validated_complexes <- append(validated_complexes, list(complex_obj))
    }
  }

  if (length(validated_complexes) > 0) {
    final_scores <- sapply(validated_complexes, function(c) c$final_score)
    order_indices <- order(final_scores, decreasing = TRUE, na.last = TRUE)
    validated_complexes <- validated_complexes[order_indices]
  }
  return(validated_complexes)
}

# --- Main Protein Complex Merging Algorithm ---

#' Merge Protein Complexes from Two Techniques
#'
#' Main function to merge protein complex lists from two sources.
#' It performs intra-list redundancy removal, cross-list merging based on
#' protein similarity, and final validation and ranking.
#'
#' @param raw_list1 A list where each element is a character vector of protein
#'   IDs, representing complexes from the first technique. Can be named or
#'   unnamed (default names will be generated if unnamed).
#' @param raw_list2 A list similar to `raw_list1` for the second technique.
#' @param list1_id Character, an identifier for `raw_list1` (e.g., "AP-MS").
#'   Default is "Technique1".
#' @param list2_id Character, an identifier for `raw_list2` (e.g., "Co-Frac").
#'   Default is "Technique2".
#' @param list1_quality_score Numeric (0-1), overall quality score for
#'   `raw_list1` (1 is best). Default is 1.0.
#' @param list2_quality_score Numeric (0-1), overall quality score for
#'   `raw_list2`. Default is 1.0.
#' @param redundancy_threshold Numeric (0-1), similarity threshold for
#'   intra-list redundancy removal. Default is 0.9.
#' @param similarity_method_intra Character, similarity method used for
#'   intra-list redundancy removal. Options: `"jaccard"` (default),
#'   `"simpson"`, `"dice"`.
#' @param merge_similarity_threshold Numeric (0-1), similarity threshold for
#'   merging complexes between `raw_list1` and `raw_list2`. Default is 0.7.
#' @param similarity_method_inter Character, the similarity metric for
#'   comparing complexes *between* lists. Options: `"simpson"` (default),
#'   `"dice"`, `"jaccard"`.
#' @param validation_min_size Integer, minimum complex size. Default is 2.
#' @param validation_max_size Integer, maximum complex size. Default is 50.
#' @param validation_bonus_merged Numeric, multiplicative bonus for merged
#'   complexes. Default 1.5.
#' @param verbose Logical, whether to print progress messages. Default `TRUE`.
#'
#' @return A list of `ProteinComplex` S3 objects, representing the final
#'   merged, validated, and ranked complexes. This list contains ALL
#'   complexes that passed validation, regardless of being merged or
#'   technique-specific.
#'
#' @export
#' @examples
#' set.seed(123)
#' list1_data <- list(
#'   `CpxA_t1` = c("P1", "P2", "P3", "P4"),
#'   `CpxB_t1` = c("P3", "P4", "P5", "P6"),
#'   `CpxOnly1` = c("P10", "P11")
#' )
#' list2_data <- list(
#'   `CpxX_t2` = c("P1", "P2", "P3", "P5"),
#'   `CpxY_t2` = c("P3", "P4", "P5", "P6", "P7", "P8"),
#'   `CpxOnly2` = c("P20", "P21")
#' )
#'
#' results <- merge_protein_complexes(
#'   raw_list1 = list1_data, raw_list2 = list2_data,
#'   list1_id = "APMS", list2_id = "CoFrac",
#'   list1_quality_score = 0.9, list2_quality_score = 0.8,
#'   merge_similarity_threshold = 0.5,
#'   similarity_method_inter = "simpson",
#'   verbose = FALSE
#' )
#' # 'results' itself contains all complexes
#' cat("Total complexes in results:", length(results), "\n")
#'
#' # Using extraction functions for specific subsets or formats
#' merged_only <- get_merged_complexes(results)
#' cat("Number of merged complexes:", length(merged_only), "\n")
#'
#' apms_specific <- get_technique_specific_complexes(results, "APMS")
#' cat("Number of APMS-specific complexes:", length(apms_specific), "\n")
#'
#' summary_counts <- get_all_complex_summary(results)
#'
#' simplified_protein_lists <- extract_all_protein_lists(results)
#' cat("Number of entries in simplified list:",
#'     length(simplified_protein_lists), "\n")
#' if (length(simplified_protein_lists) > 0) {
#'   cat("First complex (simplified):",
#'       names(simplified_protein_lists)[1], "->",
#'       paste(simplified_protein_lists[[1]], collapse=","), "\n")
#' }
#'
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

  if (list1_quality_score < 0 || list1_quality_score > 1) {
    stop("list1_quality_score must be between 0 and 1.")
  }
  if (list2_quality_score < 0 || list2_quality_score > 1) {
    stop("list2_quality_score must be between 0 and 1.")
  }
  valid_sim_methods <- c("simpson", "dice", "jaccard")
  if (!similarity_method_intra %in% valid_sim_methods) {
    stop("Invalid similarity_method_intra. Choose from: ",
         paste(valid_sim_methods, collapse = ", "))
  }
  if (!similarity_method_inter %in% valid_sim_methods) {
    stop("Invalid similarity_method_inter. Choose from: ",
         paste(valid_sim_methods, collapse = ", "))
  }

  if (verbose) message("Starting protein complex merging process...")

  raw_list1_named <- ensure_named_list(raw_list1, list1_id)
  raw_list2_named <- ensure_named_list(raw_list2, list2_id)

  s3_list1 <- convert_raw_list_to_s3(raw_list1_named, list1_id,
                                     list1_quality_score)
  s3_list2 <- convert_raw_list_to_s3(raw_list2_named, list2_id,
                                     list2_quality_score)

  if (verbose) {
    message("List 1 initial complexes: ", length(s3_list1),
            ", List 2 initial complexes: ", length(s3_list2))
  }

  if (verbose) message("Cleaning list 1 (using ", similarity_method_intra,
                       " for redundancy)...")
  cleaned_list1 <- remove_redundancy(s3_list1, redundancy_threshold,
                                     similarity_method_intra)
  if (verbose) message("Cleaned list 1 size: ", length(cleaned_list1))

  if (verbose) message("Cleaning list 2 (using ", similarity_method_intra,
                       " for redundancy)...")
  cleaned_list2 <- remove_redundancy(s3_list2, redundancy_threshold,
                                     similarity_method_intra)
  if (verbose) message("Cleaned list 2 size: ", length(cleaned_list2))

  merged_complexes_output <- list()
  used_indices_list2 <- rep(FALSE, length(cleaned_list2))

  if (verbose) message("Starting cross-technique comparison (using ",
                       similarity_method_inter, ") and merging...")
  if (length(cleaned_list1) > 0) {
    for (i in seq_along(cleaned_list1)) {
      complex1 <- cleaned_list1[[i]]
      best_match_complex2 <- NULL
      best_similarity_score <- 0
      best_match_idx2 <- -1

      if (length(cleaned_list2) > 0) {
        for (j in seq_along(cleaned_list2)) {
          if (!used_indices_list2[j]) {
            complex2 <- cleaned_list2[[j]]
            similarity <- calculate_similarity(complex1, complex2,
                                               method = similarity_method_inter)

            if (similarity > best_similarity_score &&
                similarity >= merge_similarity_threshold) {
              best_similarity_score <- similarity
              best_match_complex2 <- complex2
              best_match_idx2 <- j
            }
          }
        }
      }

      if (!is.null(best_match_complex2)) {
        if (verbose) {
          message(
            "Merging: ", complex1$original_name, " (", list1_id, ") with ",
            best_match_complex2$original_name, " (", list2_id,
            ") - Sim (", similarity_method_inter, "): ",
            round(best_similarity_score, 3)
          )
        }
        merged_c <- merge_two_complexes_internal(complex1,
                                                 best_match_complex2,
                                                 best_similarity_score)
        merged_complexes_output <- append(merged_complexes_output,
                                          list(merged_c))
        if (best_match_idx2 > 0) {
          used_indices_list2[best_match_idx2] <- TRUE
        }
      } else {
        complex1$support_level <- paste0(complex1$source_list_id, "_only")
        merged_complexes_output <- append(merged_complexes_output,
                                          list(complex1))
      }
    }
  }

  if (verbose) message("Adding remaining complexes from list 2...")
  if (length(cleaned_list2) > 0) {
    for (j in seq_along(cleaned_list2)) {
      if (!used_indices_list2[j]) {
        complex2 <- cleaned_list2[[j]]
        complex2$support_level <- paste0(complex2$source_list_id, "_only")
        merged_complexes_output <- append(merged_complexes_output,
                                          list(complex2))
      }
    }
  }

  if (verbose) {
    message("Validating and ranking ", length(merged_complexes_output),
            " complexes...")
  }
  validated_ranked_complexes <- validate_and_rank_complexes(
    merged_complexes_output,
    min_size = validation_min_size,
    max_size = validation_max_size,
    both_techniques_bonus = validation_bonus_merged
  )

  if (verbose) {
    message("Final validated and ranked complexes: ",
            length(validated_ranked_complexes))
    message("Complex merging process complete.")
  }

  return(validated_ranked_complexes)
}

# --- Functions to Extract Subsets from Results ---

#' Extract Merged Complexes
#'
#' Filters the results from `merge_protein_complexes` to return only those
#' complexes that are supported by both techniques (i.e., were merged).
#'
#' @param results A list of `ProteinComplex` S3 objects, as returned by
#'   `merge_protein_complexes`.
#' @return A list containing only the merged `ProteinComplex` S3 objects.
#'   Returns an empty list if no merged complexes are present.
#' @export
get_merged_complexes <- function(results) {
  if (!is.list(results) ||
      (length(results) > 0 && !inherits(results[[1]], "ProteinComplex"))) {
    stop("Input 'results' must be a list of ProteinComplex objects.")
  }
  Filter(function(cpx) identical(cpx$support_level, "both_techniques"),
         results)
}

#' Extract Technique-Specific Complexes
#'
#' Filters the results from `merge_protein_complexes` to return only those
#' complexes that are specific to a given technique/source list.
#'
#' @param results A list of `ProteinComplex` S3 objects, as returned by
#'   `merge_protein_complexes`.
#' @param technique_id Character, the identifier of the technique/source list
#'   (e.g., "APMS", "Technique1") for which to extract specific complexes.
#'   This should match one of the `list1_id` or `list2_id` used when calling
#'   `merge_protein_complexes`.
#' @return A list containing only the `ProteinComplex` S3 objects specific to
#'   the specified technique. Returns an empty list if no such complexes exist
#'   or if the `technique_id` is not found.
#' @export
get_technique_specific_complexes <- function(results, technique_id) {
  if (!is.list(results) ||
      (length(results) > 0 && !inherits(results[[1]], "ProteinComplex"))) {
    stop("Input 'results' must be a list of ProteinComplex objects.")
  }
  if (!is.character(technique_id) || length(technique_id) != 1) {
    stop("technique_id must be a single character string.")
  }

  expected_support_level <- paste0(technique_id, "_only")
  Filter(function(cpx) identical(cpx$support_level, expected_support_level),
         results)
}

#' Get Summary of Complex Types in Results
#'
#' Provides a count of merged complexes and technique-specific complexes for
#' each input list present in the results.
#'
#' @param results A list of `ProteinComplex` S3 objects, as returned by
#'   `merge_protein_complexes`.
#' @return A named list or vector summarizing the counts (returned invisibly
#'   after printing to console).
#' @export
get_all_complex_summary <- function(results) {
  if (!is.list(results) ||
      (length(results) > 0 && !inherits(results[[1]], "ProteinComplex"))) {
    stop("Input 'results' must be a list of ProteinComplex objects.")
  }

  support_levels <- sapply(results, function(cpx) cpx$support_level)

  counts <- list()
  counts$merged_both_techniques <-
    sum(support_levels == "both_techniques", na.rm = TRUE)

  # Dynamically find unique source list IDs from non-merged complexes
  # This ensures we only report on actual source IDs present.
  source_ids_from_specific <- sapply(results, function(cpx) {
    if (endsWith(cpx$support_level, "_only")) {
      return(cpx$source_list_id)
    }
    return(NA_character_)
  })
  unique_tech_ids <- unique(stats::na.omit(source_ids_from_specific))

  for (id in unique_tech_ids) {
    specific_level_name <- paste0(id, "_only")
    count_name <- paste0("specific_", id) # e.g., specific_APMS
    counts[[count_name]] <- sum(support_levels == specific_level_name,
                                na.rm = TRUE)
  }

  message("Complex Summary:")
  message("  Total complexes in results: ", length(results))
  if (length(counts) > 0) {
    for(name in names(counts)){
      message("  ", gsub("_", " ", name), ": ", counts[[name]])
    }
  } else {
    message("  No complexes found to summarize.")
  }

  return(invisible(counts))
}

#' Extract Simplified List of All Complexes (Proteins Only)
#'
#' Extracts a simplified named list from the results of
#' `merge_protein_complexes`. The names of the list elements are derived from
#' the `original_name` of the complexes (and optionally their support level),
#' and the values are the character vectors of their constituent proteins.
#'
#' @param results A list of `ProteinComplex` S3 objects, as returned by
#'   `merge_protein_complexes`.
#' @param include_support_level_in_name Logical. If `TRUE`, the names of the
#'   returned list will be appended with their support level
#'   (e.g., "ComplexName_both_techniques"). Default `FALSE`.
#'
#' @return A named list where names are complex identifiers and values are
#'   character vectors of proteins.
#' @export
extract_all_protein_lists <- function(results,
                                      include_support_level_in_name = FALSE) {
  if (!is.list(results) ||
      (length(results) > 0 && !inherits(results[[1]], "ProteinComplex"))) {
    stop("Input 'results' must be a list of ProteinComplex objects.")
  }

  if (length(results) == 0) {
    return(list())
  }

  protein_lists <- lapply(results, function(cpx) cpx$proteins)

  if (include_support_level_in_name) {
    complex_names <- sapply(results, function(cpx) {
      # Sanitize support_level for use in names if it contains spaces
      support_tag <- gsub(" ", "_", cpx$support_level)
      paste(cpx$original_name, support_tag, sep = "_")
    })
  } else {
    complex_names <- sapply(results, function(cpx) cpx$original_name)
  }

  # Ensure names are unique, especially if include_support_level_in_name is FALSE
  # as original_name might not be globally unique after merging (e.g. MERGED_A_B)
  if (any(duplicated(complex_names))) {
    message("Warning: Duplicate names generated for simplified list. ",
            "Using make.unique(). Consider include_support_level_in_name = TRUE.")
    complex_names <- make.unique(complex_names, sep = "_")
  }

  names(protein_lists) <- complex_names
  return(protein_lists)
}
