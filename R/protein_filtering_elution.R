#' Retain Proteins Based on Elution Profile Consistency
#'
#' Filters proteins from an elution matrix based on the consistency of their
#' elution profiles. It uses a permutation-based approach: repeatedly,
#' fractions are split into two halves, and a ratio of summed intensities
#' (or squared intensities) is calculated for each protein. Proteins whose
#' average ratio across permutations deviates significantly (based on Median
#' Absolute Deviation - MAD) from the overall median ratio are retained.
#' This method aims to identify proteins with non-random or structured
#' elution patterns.
#'
#' @param elution_matrix A numeric matrix where rows are proteins and columns
#'   are fractions. Missing values (NA) will be imputed.
#' @param num_permutations Integer, the number of permutations to perform for
#'   calculating profile consistency. Default is 1000.
#' @param mad_threshold Numeric, the number of MADs from the median ratio to use
#'   as a threshold for retaining proteins. Proteins with ratios outside
#'   `median +/- mad_threshold * MAD` are kept. Default is determined by the
#'   original code's context (e.g., `nmad` argument in `RetainElutionProtein`).
#'   A common value might be 2 or 3. Here, let's use 2.
#' @param verbose Logical, if `TRUE`, a progress bar will be displayed.
#'   Default `TRUE`.
#'
#' @return A character vector of protein identifiers (row names from the
#'   input matrix) that are retained after filtering.
#' @export
#' @importFrom stats median mad
#' @importFrom progress progress_bar
#' @examples
#' # Create a dummy elution matrix
#' set.seed(123)
#' mat <- matrix(rnorm(50 * 20), nrow = 50, ncol = 20)
#' rownames(mat) <- paste0("Prot", 1:50)
#' colnames(mat) <- paste0("Frac", 1:20)
#' # Introduce some NAs
#' mat[sample(length(mat), 50)] <- NA
#' # Add some structured profiles for a few proteins
#' mat[1, 5:10] <- mat[1, 5:10] + 5 # Prot1 peaks
#' mat[2, 10:15] <- mat[2, 10:15] + 5 # Prot2 peaks
#'
#' # Reduce permutations for quick example
#' retained_prots <- filter_proteins_by_elution_consistency(mat,
#'                                           num_permutations = 50,
#'                                           mad_threshold = 2,
#'                                           verbose = FALSE)
#' print(paste("Number of retained proteins:", length(retained_prots)))
#' # print(retained_prots) # Potentially Prot1, Prot2 and others by chance
filter_proteins_by_elution_consistency <- function(
    elution_matrix,
    num_permutations = 1000,
    mad_threshold = 2,
    verbose = TRUE) {

  if (!is.matrix(elution_matrix) || !is.numeric(elution_matrix)) {
    stop("'elution_matrix' must be a numeric matrix.")
  }
  if (nrow(elution_matrix) == 0) return(character(0))
  if (ncol(elution_matrix) < 2) {
    stop("Need at least 2 fractions (columns) for comparison.")
  }

  # Impute NAs and add pseudocount (as per original logic `m <- m + 1; m[is.na(m)] <- 1`)
  # This transformation might be specific to downstream log-transformations
  # or count-based assumptions not fully clear from this function alone.
  # We replicate it here.
  processed_matrix <- elution_matrix
  processed_matrix[is.na(processed_matrix)] <- 0 # First set NAs to 0
  processed_matrix <- processed_matrix + 1       # Then add 1 (pseudocount)

  # Ensure an even number of fractions for splitting
  num_cols <- ncol(processed_matrix)
  if (num_cols %% 2 != 0) {
    # Drop the last column if odd number of columns
    processed_matrix <- processed_matrix[, -num_cols, drop = FALSE]
    num_cols <- num_cols - 1
    if (num_cols < 2) stop("Need at least 2 fractions after ensuring even number.")
    if (verbose) message("Odd number of columns; last column dropped for splitting.")
  }
  half_cols <- num_cols / 2

  # Store ratios from permutations
  ratio_list_across_permutations <- vector("list", num_permutations)

  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Calculating consistency [:bar] :percent eta: :eta",
      total = num_permutations,
      width = 80,
      clear = FALSE
    )
  }

  for (i in seq_len(num_permutations)) {
    if (verbose) pb$tick()

    # Randomly sample columns for the 'control' half
    control_indices <- sample(seq_len(num_cols), half_cols)
    target_indices <- setdiff(seq_len(num_cols), control_indices)

    # Sum of squares for each protein in control and target halves
    # Original code: `rowSums((mDat[, idControl])^2)`
    sum_sq_control <- rowSums(processed_matrix[, control_indices, drop=FALSE]^2)
    sum_sq_target <- rowSums(processed_matrix[, target_indices, drop=FALSE]^2)

    # Calculate ratio: sqrt(sum_sq_control / sum_sq_target)
    # Add small epsilon to denominator to prevent division by zero
    ratios <- sqrt(sum_sq_control / (sum_sq_target + .Machine$double.eps))
    ratio_list_across_permutations[[i]] <- ratios
  }

  # Average ratios across permutations for each protein
  if (length(ratio_list_across_permutations) > 0) {
    ratios_df <- do.call(cbind, ratio_list_across_permutations)
    mean_ratios_per_protein <- rowMeans(ratios_df, na.rm = TRUE)
  } else {
    mean_ratios_per_protein <- numeric(nrow(processed_matrix))
    names(mean_ratios_per_protein) <- rownames(processed_matrix)
  }


  # Filter proteins: keep those whose mean ratio is far from the median
  median_of_ratios <- stats::median(mean_ratios_per_protein, na.rm = TRUE)
  mad_of_ratios <- stats::mad(mean_ratios_per_protein, na.rm = TRUE)

  # If MAD is zero (e.g., all ratios are the same), avoid issues
  if (mad_of_ratios == 0) {
    if (verbose) {
      message("MAD of ratios is zero. Thresholding cannot be applied meaningfully.",
              " Returning all proteins or none based on deviation from median.")
    }
    # If all ratios are identical, no protein stands out.
    # If some ratios differ from median but MAD is 0 due to many identical values at median,
    # this logic might still be useful.
    # For robustness: if MAD is 0, only keep proteins if their ratio is not exactly the median.
    # Or, if all are median, keep none (or all, depending on desired behavior).
    # Original logic keeps proteins if `prtDC < median - nmad*0` OR `prtDC > median + nmad*0`.
    # This means keep if `prtDC != median`.
    proteins_to_pass_filter <- names(mean_ratios_per_protein)[
      mean_ratios_per_protein != median_of_ratios
    ]
  } else {
    lower_bound <- median_of_ratios - mad_threshold * mad_of_ratios
    upper_bound <- median_of_ratios + mad_threshold * mad_of_ratios

    proteins_to_pass_filter <- names(mean_ratios_per_protein)[
      (mean_ratios_per_protein < lower_bound) |
        (mean_ratios_per_protein > upper_bound)
    ]
  }


  if (is.null(rownames(elution_matrix))) {
    warning("Elution matrix has no rownames. Returning indices instead.")
    return(which(rownames(processed_matrix) %in% proteins_to_pass_filter))
  }

  return(proteins_to_pass_filter)
}
