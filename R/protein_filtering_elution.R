#' Filter Proteins Based on Elution Profile Consistency
#'
#' Identifies and retains proteins with structured elution profiles from 
#' chromatographic fractionation data. The function evaluates profile consistency
#' using a permutation-based approach that assesses reproducibility across
#' fraction splits. This is particularly useful for identifying proteins that
#' co-elute as part of protein complexes.
#'
#' @details
#' ## Elution Profile Criteria
#' - Profiles should show consistent peak patterns across technical replicates
#' - Peaks should have sufficient width (typically 3+ fractions) to be considered
#'   biologically relevant
#' - Profiles should be reproducible across random splits of the fractions
#'
#' ## Biological Significance
#' Proteins with consistent elution profiles are more likely to:
#' - Be part of stable protein complexes
#' - Have specific interaction partners  
#' - Show coordinated regulation
#' - Represent biologically meaningful interactions rather than random co-elution
#'
#' ## Chromatographic Assumptions
#' - Fractions are ordered by elution time (early to late)
#' - Fractionation provides sufficient resolution (10-30 fractions ideal)
#' - Technical variation between fractions is minimal
#' - Peak shapes follow expected chromatographic behavior (Gaussian-like)
#'
#' ## Quality Control
#' - MAD threshold filters out proteins with random/noisy profiles
#' - Permutation approach ensures robustness to fraction ordering
#' - Missing value imputation handles common MS data issues
#' - Automatic handling of odd-numbered fraction sets
#'
#' ## Performance Characteristics
#' - Time complexity: O(n*p*k) where:
#'   - n = number of proteins
#'   - p = number of permutations  
#'   - k = number of fractions
#' - Memory usage: Scales with matrix size + permutation storage
#' - Parallelization: Not currently implemented but could benefit from it
#' - Typical runtime: ~1-5 minutes for 1000 proteins, 20 fractions, 1000 perms
#'
#' ## Parameter Effects
#' - `num_permutations`: Higher values increase precision but linearly increase runtime
#'   - Below 100: Potentially unstable results
#'   - 100-1000: Typical range for most analyses
#'   - Above 1000: Diminishing returns
#' - `mad_threshold`: Controls stringency of filtering
#'   - 0-1: Very permissive (may retain noise)
#'   - 1-2: Balanced (default)
#'   - 2-3: Stringent (may lose true complexes)
#'   - 3+: Very stringent (high specificity)
#'
#' @param elution_matrix A numeric matrix where rows are proteins and columns
#'   are fractions. Expected to contain MS intensity values (log2 recommended).
#'   Missing values (NA) will be imputed using row medians. Row names should be
#'   protein identifiers for meaningful output.
#' @param num_permutations Integer (>=10), the number of permutations to perform
#'   for calculating profile consistency. More permutations increase precision
#'   but require more computation. Default: 1000.
#' @param mad_threshold Numeric (>=0), the number of MADs from the median ratio
#'   to use as threshold. Higher values retain fewer proteins. Typical range 1-3.
#'   Default: 2.
#' @param verbose Logical, whether to show progress bar. Default: TRUE.
#'
#' @return A character vector of protein identifiers (row names from input)
#'   that pass the consistency filter. If input lacks rownames, returns numeric
#'   indices instead (with warning). The output represents proteins with
#'   reproducible elution profiles likely to be part of stable complexes.
#'
#' @seealso \code{\link{filter_proteins_by_elution_consistency}} (man page),
#'   \code{\link{prepare_elution_matrix}} for input preparation,
#'   \code{\link{filter_by_mad_threshold}} for threshold implementation
#' @export
#' @importFrom stats median mad
#' @importFrom progress progress_bar
#' @examples
#' # Create a dummy elution matrix with realistic properties
#' set.seed(123)
#' mat <- matrix(rnorm(100 * 24), nrow = 100, ncol = 24)
#' rownames(mat) <- paste0("Prot", 1:100)
#' colnames(mat) <- paste0("Frac", 1:24)
#' 
#' # Introduce structured profiles for some proteins
#' mat[1:5, 5:10] <- mat[1:5, 5:10] + rnorm(30, mean = 5, sd = 1)  # Complex A
#' mat[6:10, 12:18] <- mat[6:10, 12:18] + rnorm(42, mean = 4, sd = 1) # Complex B
#' 
#' # Add random NAs (10% missing values)
#' mat[sample(length(mat), 0.1 * length(mat))] <- NA
#' 
#' # Run filtering with different parameters
#' retained_default <- filter_proteins_by_elution_consistency(mat)
#' retained_stringent <- filter_proteins_by_elution_consistency(mat, mad_threshold = 2.5)
#' retained_fast <- filter_proteins_by_elution_consistency(mat, num_permutations = 100)
#' 
#' # Compare results
#' length(retained_default)    # Typically ~10-15 with this example
#' length(retained_stringent)  # Typically ~5-8 
#' length(retained_fast)       # Similar to default but less precise
#' 
#' # Visualize retained proteins
#' if (interactive() & requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_elution_ridges(mat[retained_default, ], ncol = 3)
#' }
filter_proteins_by_elution_consistency <- function(
    elution_matrix,
    num_permutations = 1000,
    mad_threshold = 2,
    verbose = TRUE) {
  
  # Input validation
  if (!is.matrix(elution_matrix)) {
    stop("'elution_matrix' must be a matrix.")
  }
  if (!is.numeric(elution_matrix)) {
    stop("'elution_matrix' must be numeric.")
  }
  if (nrow(elution_matrix) == 0) return(character(0))
  if (ncol(elution_matrix) < 2) {
    stop("Need at least 2 fractions (columns) for comparison.")
  }
  if (num_permutations < 10) {
    warning("Few permutations (<10) may yield unreliable results")
  }
  if (mad_threshold < 0) {
    stop("'mad_threshold' must be >= 0")
  }
  
  # Prepare matrix
  processed_matrix <- prepare_elution_matrix(elution_matrix)
  
  # Ensure even number of fractions
  num_cols <- ncol(processed_matrix)
  if (num_cols %% 2 != 0) {
    processed_matrix <- processed_matrix[, -num_cols, drop = FALSE]
    num_cols <- num_cols - 1
    if (num_cols < 2) stop("Need at least 2 fractions after ensuring even number.")
    if (verbose) message("Odd number of columns; last column dropped for splitting.")
  }
  half_cols <- num_cols / 2
  
  # Calculate ratios across permutations
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
    
    control_indices <- sample(seq_len(num_cols), half_cols)
    target_indices <- setdiff(seq_len(num_cols), control_indices)
    
    ratio_list_across_permutations[[i]] <- calculate_split_ratios(
      processed_matrix, control_indices, target_indices)
  }
  
  # Calculate mean ratios
  mean_ratios_per_protein <- if (length(ratio_list_across_permutations) > 0) {
    rowMeans(do.call(cbind, ratio_list_across_permutations), na.rm = TRUE)
  } else {
    setNames(numeric(nrow(processed_matrix)), rownames(processed_matrix))
  }
  
  # Filter proteins
  proteins_to_pass_filter <- filter_by_mad_threshold(
    mean_ratios_per_protein, mad_threshold)
  
  # Handle case where input matrix has no rownames
  if (is.null(rownames(elution_matrix))) {
    warning("Elution matrix has no rownames. Returning indices instead.")
    return(which(rownames(processed_matrix) %in% proteins_to_pass_filter))
  }
  
  return(proteins_to_pass_filter)
}
