#' ProteinComplex S3 Object Structure
#'
#' Defines the internal structure of a `ProteinComplex` S3 object used throughout
#' the complex merging pipeline. This object encapsulates all metadata about
#' protein complexes from individual techniques and merged complexes.
#'
#' @section Structure:
#' A `ProteinComplex` object is a list with these components:
#' \describe{
#'   \item{proteins}{Sorted character vector of unique protein identifiers}
#'   \item{list_quality_score_origin}{Numeric (0-1) quality score of source list}
#'   \item{original_name}{Character, original complex name/identifier}
#'   \item{source_list_id}{Character, source technique ID or "merged"}
#'   \item{support_level}{Character, indicates support level ("both_techniques",
#'     "Technique1_only", etc.)}
#'   \item{merged_from_similarity}{Numeric similarity score that triggered merge}
#'   \item{merged_stability_score}{Numeric stability score for merged complexes}
#'   \item{final_score}{Numeric ranked score after validation}
#'   \item{technique1_data}{Original `ProteinComplex` from first technique}
#'   \item{technique2_data}{Original `ProteinComplex` from second technique}
#'   \item{core_proteins}{Proteins common to both original complexes}
#'   \item{technique1_specific_proteins}{Proteins unique to first technique}
#'   \item{technique2_specific_proteins}{Proteins unique to second technique}
#'   \item{original_metadata}{List of additional metadata}
#' }
#'
#' @section Pipeline Integration:
#' This S3 class is used throughout the merging pipeline:
#' - Created by `new_protein_complex()` and `convert_raw_list_to_s3()`
#' - Processed by similarity and merging functions
#' - Validated and ranked by `validate_and_rank_complexes()`
#'
#' @seealso \code{\link{merge_protein_complexes}} for the main pipeline function
#' @seealso \code{\link{utils}} for general utility functions
#' @name ProteinComplex-S3
#' @keywords internal
NULL

#' Create a New ProteinComplex S3 Object
#'
#' Internal constructor that validates inputs and creates a properly structured
#' `ProteinComplex` object. Ensures proteins are unique and sorted.
#'
#' @param proteins Character vector of protein IDs. Duplicates will be removed.
#' @param list_quality_score_origin Numeric (0-1) quality score of source list.
#' @param original_name Character, original name of the complex.
#' @param source_list_id Character, ID of the source technique.
#' @param metadata List of additional metadata to store.
#'
#' @return A validated `ProteinComplex` S3 object with these guarantees:
#' - `proteins` are unique and sorted
#' - All fields are properly typed
#' - Required fields are initialized
#'
#' @section Internal Checks:
#' - Converts proteins to character and removes duplicates
#' - Ensures numeric fields are properly typed
#' - Initializes all required fields with NA/NULL defaults
#'
#' @examples
#' # Create a simple complex
#' simple_complex <- new_protein_complex(c("P12345", "Q98765"))
#' 
#' # Create with metadata
#' complex_with_meta <- new_protein_complex(
#'   c("P12345", "Q98765"),
#'   list_quality_score_origin = 0.8,
#'   original_name = "MyComplex",
#'   source_list_id = "AP-MS",
#'   metadata = list(experiment_id = "EXP123")
#' )
#'
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

#' Convert Raw Protein List to ProteinComplex Objects
#'
#' Transforms a raw named list of protein vectors into properly structured
#' `ProteinComplex` S3 objects with metadata. Used as the first step in the
#' merging pipeline.
#'
#' @param raw_list Named list where each element is a character vector of
#'   protein IDs. Names become complex identifiers.
#' @param list_id_name Character identifier for this technique (e.g., "AP-MS").
#' @param list_quality_score Numeric (0-1) quality score for this technique.
#'
#' @return List of `ProteinComplex` objects with:
#' - Proteins sorted and deduplicated
#' - Source technique metadata attached
#' - Original names preserved
#'
#' @section Pipeline Role:
#' - First transformation step after data loading
#' - Output feeds into redundancy removal and merging functions
#'
#' @examples
#' raw_data <- list(
#'   Complex1 = c("P12345", "Q98765", "P12345"), # Duplicate P12345
#'   Complex2 = c("O75376", "P12345")
#' )
#' 
#' s3_complexes <- convert_raw_list_to_s3(
#'   raw_data,
#'   list_id_name = "AP-MS",
#'   list_quality_score = 0.85
#' )
#'
#' @seealso \code{\link{load_cofrac_HeLaCE12_tcs}} for example data loading
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

#' Ensure Complex List Has Valid Names
#'
#' Validates and assigns default names to a list of complexes if needed.
#' Critical for maintaining traceability through the merging pipeline.
#'
#' @param complex_list List of `ProteinComplex` objects.
#' @param list_id_prefix Character prefix for default names.
#'
#' @return The input list with guaranteed valid names. If input names were:
#' - Missing/invalid: Assigns default names like "Technique1_Complex_1"
#' - Valid: Returns list unchanged
#'
#' @section Internal Checks:
#' - Verifies input is a list
#' - Checks for missing/empty/duplicate names
#' - Only modifies names if necessary
#'
#' @examples
#' unnamed_list <- list(
#'   new_protein_complex(c("P1", "P2")),
#'   new_protein_complex(c("P3", "P4")))
#' )
#' 
#' named_list <- ensure_named_list(
#'   unnamed_list,
#'   list_id_prefix = "AP-MS"
#' )
#'
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

#' Calculate Similarity Between Two Complexes
#'
#' Computes protein composition similarity using Simpson, Dice, or Jaccard index.
#' Core function used for identifying merge candidates in the pipeline.
#'
#' @param complex1 First `ProteinComplex` object.
#' @param complex2 Second `ProteinComplex` object.
#' @param method Similarity method:
#'   - "simpson": min(intersection)/min(size) [default]
#'   - "dice": 2*intersection/(size1 + size2)
#'   - "jaccard": intersection/union
#'
#' @return Numeric similarity score (0-1) where:
#' - 1 = identical protein composition
#' - 0 = no shared proteins
#' - Values between represent partial overlap
#'
#' @section Error Handling:
#' - Validates inputs are ProteinComplex objects
#' - Handles empty protein lists gracefully
#' - Provides clear error for invalid method
#'
#' @section Pipeline Role:
#' - Used by `remove_redundancy_intra_list()` for filtering
#' - Critical for identifying merge candidates
#' - Scores stored in merged complexes
#'
#' @examples
#' c1 <- new_protein_complex(c("P1", "P2", "P3"))
#' c2 <- new_protein_complex(c("P2", "P3", "P4"))
#' calculate_similarity(c1, c2, "simpson") # 0.6666667
#' calculate_similarity(c1, c2, "dice")    # 0.6666667
#' calculate_similarity(c1, c2, "jaccard") # 0.5
#'
#' @seealso \code{\link{merge_two_complexes}} for merging similar complexes
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
  
  if (len_intersect == 0) return(0.0)
  
  if (method == "simpson") {
    min_len <- min(len1, len2)
    return(len_intersect / min_len)
  } else if (method == "dice") {
    sum_len <- len1 + len2
    return((2 * len_intersect) / sum_len)
  } else if (method == "jaccard") {
    union_set <- union(proteins1, proteins2)
    return(len_intersect / length(union_set))
  }
  else {
    stop("Invalid similarity method: ", method,
         ". Choose 'simpson', 'dice', or 'jaccard'.")
  }
}

#' Remove Redundant Complexes Within a Technique
#'
#' Filters a technique's complexes to remove smaller complexes that are highly
#' similar to larger ones. Reduces redundancy before cross-technique merging.
#'
#' @param complex_s3_list List of `ProteinComplex` objects from one technique.
#' @param redundancy_threshold Numeric (0-1), similarity threshold for considering
#'   a smaller complex redundant (default: 0.9).
#' @param similarity_method_intra Similarity method ("simpson", "dice", "jaccard").
#'
#' @return Filtered list where smaller redundant complexes have been removed.
#'   Larger complexes are always retained.
#'
#' @section Algorithm Details:
#' 1. Orders complexes by size (largest first)
#' 2. Compares each complex to kept complexes
#' 3. Keeps only if similarity < threshold to all kept complexes
#'
#' @section Pipeline Role:
#' - Applied to each technique's complexes separately
#' - Reduces computational load for cross-technique merging
#' - Preserves most representative complexes
#'
#' @examples
#' c1 <- new_protein_complex(c("P1", "P2", "P3", "P4"))
#' c2 <- new_protein_complex(c("P1", "P2", "P3")) # Similar to c1
#' c3 <- new_protein_complex(c("P5", "P6")) # Different
#' filtered <- remove_redundancy_intra_list(list(c1, c2, c3), 0.8) # Removes c2
#'
#' @seealso \code{\link{calculate_similarity}} for similarity calculations
#' @keywords internal
#' @noRd
remove_redundancy_intra_list <- function(complex_s3_list,
                                         redundancy_threshold = 0.9,
                                         similarity_method_intra = "jaccard") {
  if (length(complex_s3_list) == 0) return(list())
  
  sizes <- vapply(complex_s3_list,
                  function(c) length(c$proteins), integer(1))
  order_indices <- order(sizes, decreasing = TRUE)
  sorted_complexes <- complex_s3_list[order_indices]
  
  final_cleaned_list <- list()
  
  for (i in seq_along(sorted_complexes)) {
    current_complex <- sorted_complexes[[i]]
    is_redundant_with_kept <- FALSE
    
    if (length(final_cleaned_list) > 0) {
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
      final_cleaned_list <- append(final_cleaned_list, list(current_complex))
    }
  }
  return(final_cleaned_list)
}

#' Merge Two Similar ProteinComplex Objects
#'
#' Combines two complexes into a new merged complex with all proteins and merge
#' metadata. Core function of the cross-technique merging pipeline.
#'
#' @param complex1 First `ProteinComplex` to merge.
#' @param complex2 Second `ProteinComplex` to merge.
#' @param similarity_score Numeric (0-1) similarity that triggered the merge.
#'
#' @return New `ProteinComplex` with:
#' - All unique proteins from both complexes
#' - Merge metadata including:
#'   - Original complexes stored in technique1/2_data
#'   - Core and technique-specific proteins identified
#'   - Calculated stability score
#'
#' @section Merge Process:
#' 1. Combines all unique proteins
#' 2. Creates merged name from originals
#' 3. Calculates stability score as average of input quality scores
#' 4. Identifies core and technique-specific proteins
#'
#' @section Pipeline Role:
#' - Core merging operation
#' - Called after similarity threshold is met
#' - Output forms the merged complex catalog
#'
#' @examples
#' c1 <- new_protein_complex(c("P1", "P2", "P3"), source_list_id = "Tech1")
#' c2 <- new_protein_complex(c("P2", "P3", "P4"), source_list_id = "Tech2")
#' merged <- merge_two_complexes(c1, c2, 0.67)
#' merged$proteins # c("P1", "P2", "P3", "P4")
#' merged$core_proteins # c("P2", "P3")
#'
#' @seealso \code{\link{validate_and_rank_complexes}} for post-merge processing
#' @keywords internal
#' @noRd
merge_two_complexes <- function(complex1, complex2,
                               similarity_score) {
  
  merged_proteins <- sort(unique(union(complex1$proteins, complex2$proteins)))
  
  c1_name_short <- substr(complex1$original_name, 1, 15)
  c2_name_short <- substr(complex2$original_name, 1, 15)
  merged_original_name <- paste("MERGED", c1_name_short, c2_name_short,
                               sep = "_")
  merged_original_name <- substr(merged_original_name, 1, 60)
  
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

#' Validate and Rank Protein Complexes
#'
#' Final processing step that filters complexes by size and assigns ranking
#' scores. Gives merged complexes a bonus for multi-technique support.
#'
#' @param all_complexes_list Combined list of merged and unmerged complexes.
#' @param min_size Minimum protein count (default: 2).
#' @param max_size Maximum protein count (default: 50).
#' @param both_techniques_bonus Score multiplier for merged complexes (default: 1.5).
#'
#' @return Filtered and ranked list of complexes sorted by:
#' 1. Merged complexes (with bonus) first
#' 2. Then by quality score descending
#'
#' @section Validation Criteria:
#' - Size between min_size and max_size
#' - Merged complexes get bonus multiplier
#' - Original quality scores used for unmerged complexes
#'
#' @section Pipeline Role:
#' - Final quality control step
#' - Produces the ranked complex catalog
#' - Called after all merging is complete
#'
#' @examples
#' c1 <- new_protein_complex(c("P1", "P2"), list_quality_score_origin = 0.8)
#' c2 <- new_protein_complex(c("P3", "P4", "P5"), list_quality_score_origin = 0.9)
#' merged <- merge_two_complexes(c1, c2, 0.7)
#' 
#' ranked <- validate_and_rank_complexes(list(c1, c2, merged))
#' # merged complex will appear first due to bonus
#'
#' @seealso \code{\link{get_merged_complexes}} for accessing final results
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
    final_scores <- vapply(validated_complexes,
                           function(c) c$final_score, numeric(1))
    order_indices <- order(final_scores, decreasing = TRUE, na.last = TRUE)
    validated_complexes <- validated_complexes[order_indices]
  }
  return(validated_complexes)
}
