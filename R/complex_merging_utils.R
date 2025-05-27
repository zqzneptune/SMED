# --- S3 Class Definition and Helpers for ProteinComplex ---

#' ProteinComplex S3 Object Structure
#'
#' This documentation describes the internal structure of a `ProteinComplex` S3
#' object used within the complex merging functions. Users typically do not
#' need to interact with this structure directly but should use the provided
#' merging and accessor functions.
#'
#' A `ProteinComplex` object is a list with the following key components:
#' \describe{
#'   \item{proteins}{A sorted character vector of unique protein identifiers
#'     constituting the complex.}
#'   \item{list_quality_score_origin}{Numeric, the quality score (0-1) of the
#'     source list from which this complex originated or the averaged score if
#'     merged.}
#'   \item{original_name}{Character, the original name or identifier of the
#'     complex from its source list, or a generated name for merged complexes.}
#'   \item{source_list_id}{Character, identifier of the source list (e.g.,
#'     "Technique1", "APMS") or "merged" if the complex results from merging.}
#'   \item{support_level}{Character, indicates the support:
#'     "both_techniques" if merged, or e.g., "Technique1_only" if specific.}
#'   \item{merged_from_similarity}{Numeric, the similarity score (e.g., Simpson)
#'     that led to this complex being merged. `NA` if not a merged complex.}
#'   \item{merged_stability_score}{Numeric, a stability score for merged
#'     complexes, often an average of the origin list quality scores. `NA` if
#'     not merged.}
#'   \item{final_score}{Numeric, a ranked score after validation, potentially
#'     including bonuses for merged complexes.}
#'   \item{technique1_data}{If merged, a `ProteinComplex` object representing
#'     the original complex from the first list. `NULL` otherwise.}
#'   \item{technique2_data}{If merged, a `ProteinComplex` object representing
#'     the original complex from the second list. `NULL` otherwise.}
#'   \item{core_proteins}{Character vector of proteins common to both original
#'     complexes if merged.}
#'   \item{technique1_specific_proteins}{Character vector of proteins unique to
#'     the first original complex if merged.}
#'   \item{technique2_specific_proteins}{Character vector of proteins unique to
#'     the second original complex if merged.}
#'   \item{original_metadata}{A list for storing any additional metadata
#'     associated with the complex from its origin.}
#' }
#' @name ProteinComplex-S3
#' @keywords internal
NULL


#' Create a New ProteinComplex S3 Object
#'
#' Internal constructor for `ProteinComplex` objects.
#'
#' @param proteins Character vector of protein IDs.
#' @param list_quality_score_origin Numeric, quality score of the source list.
#' @param original_name Character, original name of the complex.
#' @param source_list_id Character, ID of the source list.
#' @param metadata List, additional metadata.
#' @return A `ProteinComplex` S3 object.
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
#'
#' Internal helper to transform a raw list of protein vectors into a list of
#' `ProteinComplex` S3 objects.
#'
#' @param raw_list A named list where each element is a character vector of
#'   protein IDs.
#' @param list_id_name Character, identifier for this list (e.g., "Technique1").
#' @param list_quality_score Numeric (0-1), quality score for this list.
#' @return A list of `ProteinComplex` S3 objects.
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
  # Preserve names from raw_list if they were meaningful
  names(s3_list) <- names(raw_list)
  return(s3_list)
}

#' Ensure Input List is Named
#'
#' Internal helper to assign default names to an unnamed list of complexes.
#'
#' @param complex_list A list of complexes.
#' @param list_id_prefix Character string to prefix default names.
#' @return The input `complex_list`, guaranteed to have names.
#' @keywords internal
#' @noRd
ensure_named_list <- function(complex_list, list_id_prefix) {
  if (!is.list(complex_list)) {
    stop("Input must be a list.")
  }
  current_names <- names(complex_list)
  n_complexes <- length(complex_list)

  # Check if names are present, complete, not empty, and unique
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

#' Calculate Similarity Between Two Complexes
#'
#' Internal function to compute similarity (Simpson, Dice, or Jaccard)
#' between two `ProteinComplex` objects.
#'
#' @param complex1 A `ProteinComplex` object.
#' @param complex2 Another `ProteinComplex` object.
#' @param method Character, similarity method: "simpson", "dice", or "jaccard".
#' @return Numeric similarity score (0-1).
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

  # Handle empty complexes
  if (len1 == 0 && len2 == 0) return(1.0) # Both empty, considered identical
  if (len1 == 0 || len2 == 0) return(0.0) # One empty, other not, no similarity

  intersection_set <- intersect(proteins1, proteins2)
  len_intersect <- length(intersection_set)

  if (len_intersect == 0) return(0.0) # No common proteins

  if (method == "simpson") {
    min_len <- min(len1, len2)
    # min_len should not be 0 here due to earlier checks if len_intersect > 0
    return(len_intersect / min_len)
  } else if (method == "dice") {
    sum_len <- len1 + len2
    # sum_len should not be 0 here
    return((2 * len_intersect) / sum_len)
  } else if (method == "jaccard") {
    union_set <- union(proteins1, proteins2)
    # length(union_set) should not be 0 here
    return(len_intersect / length(union_set))
  }
  else {
    stop("Invalid similarity calculation method: ", method,
         ". Choose 'simpson', 'dice', or 'jaccard'.")
  }
}

#' Remove Redundant Complexes Within a Single List
#'
#' Internal function to filter a list of `ProteinComplex` objects, removing
#' complexes that are highly similar to larger, already retained complexes.
#'
#' @param complex_s3_list A list of `ProteinComplex` objects.
#' @param redundancy_threshold Numeric (0-1), similarity threshold.
#' @param similarity_method_intra Character, method for similarity calculation.
#' @return A list of `ProteinComplex` objects with redundancy removed.
#' @keywords internal
#' @noRd
remove_redundancy_intra_list <- function(complex_s3_list,
                                         redundancy_threshold = 0.9,
                                         similarity_method_intra = "jaccard") {
  if (length(complex_s3_list) == 0) return(list())

  # Sort complexes by size (descending) to prioritize larger ones
  sizes <- vapply(complex_s3_list,
                  function(c) length(c$proteins), integer(1))
  order_indices <- order(sizes, decreasing = TRUE)
  sorted_complexes <- complex_s3_list[order_indices]

  final_cleaned_list <- list()

  for (i in seq_along(sorted_complexes)) {
    current_complex <- sorted_complexes[[i]]
    is_redundant_with_kept <- FALSE

    if (length(final_cleaned_list) > 0) {
      # Check against already kept complexes
      for (kept_idx in seq_along(final_cleaned_list)) {
        kept_complex <- final_cleaned_list[[kept_idx]]
        similarity <- calculate_similarity(current_complex, kept_complex,
                                           method = similarity_method_intra)
        if (similarity >= redundancy_threshold) {
          is_redundant_with_kept <- TRUE
          break
        }
      }
    }

    if (!is_redundant_with_kept) {
      # Add current_complex to the list of final, non-redundant complexes
      final_cleaned_list <- append(final_cleaned_list, list(current_complex))
    }
  }
  return(final_cleaned_list)
}

#' Merge Two Similar ProteinComplex Objects
#'
#' Internal function to combine two `ProteinComplex` objects into a new,
#' merged `ProteinComplex` object.
#'
#' @param complex1 First `ProteinComplex` object.
#' @param complex2 Second `ProteinComplex` object.
#' @param similarity_score Numeric, the similarity score that led to merging.
#' @return A new, merged `ProteinComplex` object.
#' @keywords internal
#' @noRd
merge_two_complexes <- function(complex1, complex2,
                                similarity_score) {

  merged_proteins <- sort(unique(union(complex1$proteins, complex2$proteins)))

  # Create a concise name for the merged complex
  c1_name_short <- substr(complex1$original_name, 1, 15)
  c2_name_short <- substr(complex2$original_name, 1, 15)
  merged_original_name <- paste("MERGED", c1_name_short, c2_name_short,
                                sep = "_")
  # Ensure name is not excessively long
  merged_original_name <- substr(merged_original_name, 1, 60)


  q1 <- complex1$list_quality_score_origin
  q2 <- complex2$list_quality_score_origin
  # Weighted average if scores are different, simple average if same
  # Or simpler: just average them.
  calculated_merged_stability <- (q1 + q2) / 2

  merged_obj <- new_protein_complex(
    proteins = merged_proteins,
    list_quality_score_origin = calculated_merged_stability, # Use stability
    original_name = merged_original_name,
    source_list_id = "merged" # Special ID for merged complexes
  )

  merged_obj$support_level <- "both_techniques"
  merged_obj$merged_from_similarity <- similarity_score
  merged_obj$merged_stability_score <- calculated_merged_stability

  # Store original complexes for traceability
  merged_obj$technique1_data <- complex1
  merged_obj$technique2_data <- complex2

  # Calculate core and specific proteins
  merged_obj$core_proteins <-
    sort(unique(intersect(complex1$proteins, complex2$proteins)))
  merged_obj$technique1_specific_proteins <-
    sort(unique(setdiff(complex1$proteins, complex2$proteins)))
  merged_obj$technique2_specific_proteins <-
    sort(unique(setdiff(complex2$proteins, complex1$proteins)))

  return(merged_obj)
}

#' Validate and Rank Merged and Non-Merged Complexes
#'
#' Internal function to filter complexes by size and assign a final score
#' for ranking.
#'
#' @param all_complexes_list List of `ProteinComplex` S3 objects (merged and
#'   unmerged).
#' @param min_size Integer, minimum complex size.
#' @param max_size Integer, maximum complex size.
#' @param both_techniques_bonus Numeric, multiplicative bonus for merged
#'   complexes.
#' @return A list of validated and ranked `ProteinComplex` objects.
#' @keywords internal
#' @noRd
validate_and_rank_complexes <- function(all_complexes_list,
                                        min_size = 2,
                                        max_size = 50,
                                        both_techniques_bonus = 1.5) {
  validated_complexes <- list()

  for (complex_obj in all_complexes_list) {
    num_proteins <- length(complex_obj$proteins)
    if (num_proteins >= min_size && num_proteins <= max_size) {

      # Assign final score
      if (identical(complex_obj$support_level, "both_techniques")) {
        # For merged complexes, use their stability score + bonus
        complex_obj$final_score <-
          complex_obj$merged_stability_score * both_techniques_bonus
      } else {
        # For technique-specific complexes, use their original list quality
        complex_obj$final_score <- complex_obj$list_quality_score_origin
      }
      validated_complexes <- append(validated_complexes, list(complex_obj))
    }
  }

  # Rank by final_score (descending)
  if (length(validated_complexes) > 0) {
    final_scores <- vapply(validated_complexes,
                           function(c) c$final_score, numeric(1))
    # Handle NAs in scores, put them last
    order_indices <- order(final_scores, decreasing = TRUE, na.last = TRUE)
    validated_complexes <- validated_complexes[order_indices]
  }
  return(validated_complexes)
}
