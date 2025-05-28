#' Extract Merged Protein Complexes from Merging Results
#'
#' Filters the output of `merge_protein_complexes` to retrieve only those
#' complexes that are supported by both experimental techniques (i.e., they were
#' successfully merged). These represent high-confidence complexes with 
#' reproducible evidence across orthogonal experimental approaches.
#'
#' @section Biological Relevance:
#' Merged complexes indicate biological complexes that are robustly detected
#' across different experimental techniques (e.g., AP-MS and co-fractionation).
#' These are particularly valuable for downstream analyses as they represent
#' high-confidence protein interactions with reduced false positive rates.
#'
#' @section Performance:
#' Time complexity: O(n) where n is number of input complexes. Memory efficient
#' as it only returns references to existing objects without copying data.
#'
#' @param merging_results A list of `ProteinComplex` S3 objects, typically the
#'   output from `merge_protein_complexes`. Each complex must have:
#'   - `support_level` field indicating merging status
#'   - `proteins` field containing protein members
#'   - `original_name` identifying the complex
#' @return A list containing only the merged `ProteinComplex` S3 objects with
#'   `support_level = "both_techniques"`. Returns an empty list if no merged
#'   complexes are present or if input is empty.
#' @export
#' @seealso \code{\link{merge_protein_complexes}}, 
#'   \code{\link{get_technique_specific_complexes}},
#'   \code{\link{extract_all_protein_lists}}
#' @examples
#' \dontrun{
#' # Load example data from Havugimana et al. 2012
#' data(load_cofrac_293NE12_hcw)
#' data(load_cofrac_HeLaCE12_tcs)
#' 
#' # Merge complexes from two techniques
#' merged_results <- merge_protein_complexes(
#'   list1 = load_cofrac_293NE12_hcw(),
#'   list2 = load_cofrac_HeLaCE12_tcs(),
#'   list1_id = "HCW",
#'   list2_id = "TCS"
#' )
#'
#' # Extract only merged complexes
#' merged_complexes <- get_merged_complexes(merged_results)
#' 
#' # Quality control: Verify expected number of merged complexes
#' if (length(merged_complexes) < 50) {
#'   warning("Low number of merged complexes - check input quality")
#' }
#' }
get_merged_complexes <- function(merging_results) {
  if (!is.list(merging_results)) {
    stop("Input 'merging_results' must be a list.")
  }
  if (length(merging_results) > 0 &&
      !inherits(merging_results[[1]], "ProteinComplex")) {
    stop("Input 'merging_results' must be a list of ProteinComplex objects.")
  }

  Filter(function(cpx) identical(cpx$support_level, "both_techniques"),
         merging_results)
}

#' Extract Technique-Specific Protein Complexes
#'
#' Filters the output of `merge_protein_complexes` to retrieve complexes that are
#' specific to a single experimental technique. These represent technique-specific
#' findings that may include both true biological complexes unique to that 
#' technique and potential false positives.
#'
#' @section Biological Relevance:
#' Technique-specific complexes can reveal:
#' 1. Biological complexes uniquely present in certain conditions/cell types
#' 2. Technique-specific artifacts that require orthogonal validation
#' 3. Differences in complex composition detection between techniques
#'
#' @section Performance:
#' Time complexity: O(n) where n is number of input complexes. Memory efficient
#' as it only returns references to existing objects without copying data.
#'
#' @param merging_results A list of `ProteinComplex` S3 objects, typically the
#'   output from `merge_protein_complexes`. Each complex must have:
#'   - `support_level` field indicating technique specificity
#'   - `source_list_id` identifying the original technique
#' @param technique_id Character string identifying the technique/source list
#'   (e.g., "APMS", "HCW", "TCS"). Must match one of the `list1_id` or `list2_id`
#'   arguments used in `merge_protein_complexes`.
#' @return A list containing only the `ProteinComplex` S3 objects specific to
#'   the specified technique (with `support_level = "<technique_id>_only"`).
#'   Returns an empty list if no such complexes exist or if invalid technique_id.
#' @export
#' @seealso \code{\link{merge_protein_complexes}}, 
#'   \code{\link{get_merged_complexes}},
#'   \code{\link{extract_all_protein_lists}}
#' @examples
#' \dontrun{
#' # Using Havugimana et al. 2012 data
#' data(load_cofrac_293NE12_hcw)
#' data(load_cofrac_HeLaCE12_tcs)
#' 
#' merged_results <- merge_protein_complexes(
#'   list1 = load_cofrac_293NE12_hcw(),
#'   list2 = load_cofrac_HeLaCE12_tcs(),
#'   list1_id = "HCW",
#'   list2_id = "TCS"
#' )
#'
#' # Extract HCW-specific complexes
#' hcw_specific <- get_technique_specific_complexes(merged_results, "HCW")
#' 
#' # Quality control: Check if specific complexes make biological sense
#' if (length(hcw_specific) > 100) {
#'   message("Large number of technique-specific complexes -",
#'           " consider validating with orthogonal data")
#' }
#' }
get_technique_specific_complexes <- function(merging_results, technique_id) {
  if (!is.list(merging_results)) {
    stop("Input 'merging_results' must be a list.")
  }
  if (length(merging_results) > 0 &&
      !inherits(merging_results[[1]], "ProteinComplex")) {
    stop("Input 'merging_results' must be a list of ProteinComplex objects.")
  }
  if (!is.character(technique_id) || length(technique_id) != 1 ||
      nchar(technique_id) == 0) {
    stop("'technique_id' must be a single non-empty character string.")
  }

  expected_support_level <- paste0(technique_id, "_only")
  Filter(function(cpx) identical(cpx$support_level, expected_support_level),
         merging_results)
}

#' Summary of Protein Complex Types from Merging Results
#'
#' Provides quantitative summary of complex types (merged vs technique-specific)
#' from protein complex merging results. Essential for quality control and
#' reporting the success of complex merging across techniques.
#'
#' @section Biological Relevance:
#' The summary statistics help assess:
#' 1. Reproducibility between techniques (high merged count = good reproducibility)
#' 2. Technique-specific biases (large differences in technique-specific counts)
#' 3. Overall data quality (very low merged counts may indicate poor data)
#'
#' @section Performance:
#' Time complexity: O(n) where n is number of input complexes. Requires 
#' additional O(k) memory where k is number of unique technique IDs.
#'
#' @param merging_results A list of `ProteinComplex` S3 objects with:
#'   - `support_level` indicating merging status
#'   - `source_list_id` for technique-specific complexes
#' @return Invisibly returns a named list containing counts:
#'   \item{total_complexes_in_results}{Total input complexes}
#'   \item{merged_both_techniques}{Merged complexes count}
#'   \item{specific_<technique_id>}{Counts for each technique-specific group}
#' @export
#' @seealso \code{\link{merge_protein_complexes}},
#'   \code{\link{get_technique_specific_complexes}}
#' @examples
#' \dontrun{
#' data(load_cofrac_293NE12_hcw)
#' data(load_cofrac_HeLaCE12_tcs)
#' 
#' merged_results <- merge_protein_complexes(
#'   list1 = load_cofrac_293NE12_hcw(),
#'   list2 = load_cofrac_HeLaCE12_tcs(),
#'   list1_id = "HCW",
#'   list2_id = "TCS"
#' )
#'
#' # Get summary statistics
#' summary_counts <- get_all_complex_summary(merged_results)
#' 
#' # Quality thresholds based on expected behavior
#' if (summary_counts$merged_both_techniques/summary_counts$total_complexes_in_results < 0.2) {
#'   warning("Low merging rate - check technique compatibility")
#' }
#' }
get_all_complex_summary <- function(merging_results) {
  if (!is.list(merging_results)) {
    stop("Input 'merging_results' must be a list.")
  }
  if (length(merging_results) > 0 &&
      !inherits(merging_results[[1]], "ProteinComplex")) {
    stop("Input 'merging_results' must be a list of ProteinComplex objects.")
  }

  support_levels <- vapply(merging_results,
                          function(cpx) cpx$support_level, character(1))

  counts <- list()
  counts$total_complexes_in_results <- length(merging_results)
  counts$merged_both_techniques <-
    sum(support_levels == "both_techniques", na.rm = TRUE)

  # Dynamically find unique source list IDs from technique-specific complexes
  source_ids_from_specific <- vapply(merging_results, function(cpx) {
    if (endsWith(cpx$support_level, "_only")) {
      return(cpx$source_list_id)
    }
    return(NA_character_)
  }, character(1))
  unique_tech_ids <- unique(stats::na.omit(source_ids_from_specific))

  for (id_val in unique_tech_ids) {
    specific_level_name <- paste0(id_val, "_only")
    count_name_suffix <- gsub("[^[:alnum:]_]", "_", id_val)
    count_name <- paste0("specific_", count_name_suffix)
    counts[[count_name]] <- sum(support_levels == specific_level_name,
                               na.rm = TRUE)
  }

  message("Complex Summary:")
  message("  Total complexes in results: ", counts$total_complexes_in_results)
  message("  Merged (both techniques):   ", counts$merged_both_techniques)

  for (id_val in unique_tech_ids) {
    specific_level_name <- paste0(id_val, "_only")
    count_name_suffix <- gsub("[^[:alnum:]_]", "_", id_val)
    count_name <- paste0("specific_", count_name_suffix)
    message("  Specific to '", id_val, "':      ", counts[[count_name]])
  }
  if (length(unique_tech_ids) == 0 &&
      counts$merged_both_techniques == 0 &&
      counts$total_complexes_in_results > 0) {
    message("  Note: Complexes exist but could not be categorized as merged",
            " or specific to known technique IDs based on support_level.")
  }

  return(invisible(counts))
}

#' Extract Simplified Protein Lists from Complexes
#'
#' Converts protein complexes into simplified named lists of protein members.
#' Optimized for downstream analyses requiring simple protein-complex mappings.
#'
#' @section Biological Relevance:
#' The simplified format is essential for:
#' 1. Enrichment analyses (GO, pathway, etc.)
#' 2. Network visualization tools
#' 3. Comparing complex compositions across conditions
#'
#' @section Performance:
#' Time complexity: O(n) where n is number of input complexes. Creates new
#' list objects so memory usage scales with input size.
#'
#' @param merging_results A list of `ProteinComplex` S3 objects with:
#'   - `proteins` field containing protein members
#'   - `original_name` identifying the complex
#'   - `support_level` indicating merging status
#' @param include_support_in_name Logical indicating whether to append support
#'   level to complex names (helps distinguish merged vs specific complexes).
#'   Default `FALSE`.
#' @return A named list where each element is a character vector of protein IDs
#'   and names are either original complex names or names with support level
#'   appended. Guaranteed to have unique names.
#' @export
#' @seealso \code{\link{merge_protein_complexes}},
#'   \code{\link{get_merged_complexes}},
#'   \code{\link{get_technique_specific_complexes}}
#' @examples
#' \dontrun{
#' data(load_cofrac_293NE12_hcw)
#' data(load_cofrac_HeLaCE12_tcs)
#' 
#' merged_results <- merge_protein_complexes(
#'   list1 = load_cofrac_293NE12_hcw(),
#'   list2 = load_cofrac_HeLaCE12_tcs(),
#'   list1_id = "HCW",
#'   list2_id = "TCS"
#' )
#'
#' # Basic protein list extraction
#' protein_lists <- extract_all_protein_lists(merged_results)
#' 
#' # With support level in names
#' detailed_lists <- extract_all_protein_lists(merged_results,
#'                                           include_support_in_name = TRUE)
#' 
#' # Quality control: Check for empty complexes
#' if (any(lengths(protein_lists) == 0)) {
#'   warning("Empty complexes detected - check input data quality")
#' }
#' }
extract_all_protein_lists <- function(merging_results,
                                     include_support_in_name = FALSE) {
  if (!is.list(merging_results)) {
    stop("Input 'merging_results' must be a list.")
  }
  if (length(merging_results) > 0 &&
      !inherits(merging_results[[1]], "ProteinComplex")) {
    stop("Input 'merging_results' must be a list of ProteinComplex objects.")
  }

  if (length(merging_results) == 0) {
    return(list())
  }

  protein_lists <- lapply(merging_results, function(cpx) cpx$proteins)

  if (include_support_in_name) {
    complex_names <- vapply(merging_results, function(cpx) {
      support_tag <- gsub(" ", "_", cpx$support_level)
      support_tag <- gsub("[^a-zA-Z0-9_]", "", support_tag)
      paste(cpx$original_name, support_tag, sep = "_")
    }, character(1))
  } else {
    complex_names <- vapply(merging_results,
                           function(cpx) cpx$original_name, character(1))
  }

  if (any(duplicated(complex_names))) {
    message("Duplicate names detected for simplified list. ",
            "Applying make.unique() to ensure uniqueness.")
    complex_names <- make.unique(complex_names, sep = "_")
  }

  names(protein_lists) <- complex_names
  return(protein_lists)
}
