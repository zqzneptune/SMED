#' Extract Merged Complexes from Merging Results
#'
#' Filters the output of `merge_protein_complexes` to retrieve only those
#' complexes that are marked as being supported by both techniques (i.e.,
#' they were successfully merged).
#'
#' @param merging_results A list of `ProteinComplex` S3 objects, typically the
#'   output from `merge_protein_complexes`.
#' @return A list containing only the merged `ProteinComplex` S3 objects.
#'   Returns an empty list if no merged complexes are present or if input is
#'   empty.
#' @export
#' @seealso \code{\link{merge_protein_complexes}}
#' @examples
#' # Assuming 'results' is output from merge_protein_complexes example
#' # merged_only <- get_merged_complexes(results)
#' # cat("Number of merged complexes:", length(merged_only), "\n")
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

#' Extract Technique-Specific Complexes from Merging Results
#'
#' Filters the output of `merge_protein_complexes` to retrieve complexes
#' that are specific to a single technique/source list.
#'
#' @param merging_results A list of `ProteinComplex` S3 objects, typically the
#'   output from `merge_protein_complexes`.
#' @param technique_id Character string, the identifier of the technique/source
#'   list (e.g., "APMS", "Technique1") for which to extract specific complexes.
#'   This ID should match one of the `list1_id` or `list2_id` arguments
#'   used when calling `merge_protein_complexes`.
#' @return A list containing only the `ProteinComplex` S3 objects specific to
#'   the specified technique. Returns an empty list if no such complexes exist
#'   or if the `technique_id` is not found among specific complexes.
#' @export
#' @seealso \code{\link{merge_protein_complexes}}
#' @examples
#' # Assuming 'results' is output from merge_protein_complexes example
#' # apms_specific <- get_technique_specific_complexes(results, "APMS")
#' # cat("Number of APMS-specific complexes:", length(apms_specific), "\n")
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

#' Get Summary of Complex Types from Merging Results
#'
#' Provides a console printout and an invisible named list summarizing the
#' counts of merged complexes and technique-specific complexes based on the
#' output from `merge_protein_complexes`.
#'
#' @param merging_results A list of `ProteinComplex` S3 objects.
#' @return Invisibly returns a named list containing counts:
#'   \item{total_complexes_in_results}{Total number of complexes.}
#'   \item{merged_both_techniques}{Number of merged complexes.}
#'   \item{specific_<technique_id>}{Number of specific complexes for each
#'     identified technique ID. Multiple such entries can exist.}
#' @export
#' @seealso \code{\link{merge_protein_complexes}}
#' @examples
#' # Assuming 'results' is output from merge_protein_complexes example
#' # summary_counts <- get_all_complex_summary(results)
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
    # source_list_id is set even for specific complexes
    if (endsWith(cpx$support_level, "_only")) {
      return(cpx$source_list_id)
    }
    return(NA_character_)
  }, character(1))
  unique_tech_ids <- unique(stats::na.omit(source_ids_from_specific))

  for (id_val in unique_tech_ids) {
    specific_level_name <- paste0(id_val, "_only")
    # Sanitize id_val for use in names (e.g. replace spaces, dashes)
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

#' Extract Simplified List of All Complexes (Proteins Only)
#'
#' Converts the list of `ProteinComplex` S3 objects (from
#' `merge_protein_complexes`) into a simpler named list. Each element of the
#' output list is a character vector of protein IDs for a complex, and the
#' names are derived from the complex's `original_name`.
#'
#' @param merging_results A list of `ProteinComplex` S3 objects.
#' @param include_support_in_name Logical. If `TRUE`, the names of the
#'   returned list elements will be a combination of the complex's original
#'   name and its support level (e.g., "ComplexName_both_techniques").
#'   This helps ensure unique names. Default `FALSE`.
#'
#' @return A named list where each name is a complex identifier and each value
#'   is a character vector of protein IDs for that complex.
#' @export
#' @seealso \code{\link{merge_protein_complexes}}
#' @examples
#' # Assuming 'results' is output from merge_protein_complexes example
#' # simplified_list <- extract_all_protein_lists(results)
#' # if (length(simplified_list) > 0) {
#' #   print(names(simplified_list)[1])
#' #   print(simplified_list[[1]])
#' # }
#' #
#' # simplified_list_detailed_names <- extract_all_protein_lists(results,
#' #                                          include_support_in_name = TRUE)
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
      support_tag <- gsub(" ", "_", cpx$support_level) # Sanitize
      support_tag <- gsub("[^a-zA-Z0-9_]", "", support_tag) # Further sanitize
      paste(cpx$original_name, support_tag, sep = "_")
    }, character(1))
  } else {
    complex_names <- vapply(merging_results,
                            function(cpx) cpx$original_name, character(1))
  }

  # Ensure names are unique, critical if original_name might not be unique
  # or if include_support_in_name is FALSE.
  if (any(duplicated(complex_names))) {
    message("Warning: Duplicate names detected for simplified list. ",
            "Applying make.unique() to ensure uniqueness.")
    complex_names <- make.unique(complex_names, sep = "_")
  }

  names(protein_lists) <- complex_names
  return(protein_lists)
}
