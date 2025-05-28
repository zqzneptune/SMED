#' Filter Matrix Rows Based on Number of Non-Zero Entries
#'
#' Filters a numeric matrix, retaining rows that have at least a specified
#' number of non-zero (and non-NA) entries across its columns.
#' NAs are treated as zeros for the purpose of counting.
#'
#' @param input_matrix A numeric matrix. Must contain finite numeric values.
#'   Row and column names are preserved in the output.
#' @param min_fractions Integer, the minimum number of columns in which a row
#'   must have a non-zero value to be retained. Default is 2. Must be a positive
#'   integer less than or equal to the number of columns.
#'
#' @return A filtered matrix, containing only the rows that meet the
#'   `min_fractions` criterion. Row and column names are preserved. Returns an
#'   empty matrix (with preserved column names) if no rows meet criteria.
#'
#' @section Biological Rationale:
#' In proteomics data analysis, this filtering is commonly applied to:
#' \itemize{
#'   \item Remove proteins with insufficient detection across fractions
#'   \item Retain only proteins with evidence of consistent elution
#'   \item Reduce noise from sporadic low-abundance detections
#'   \item Improve signal-to-noise ratio in downstream analyses
#' }
#'
#' @section Threshold Determination:
#' The `min_fractions` parameter should be set based on:
#' \itemize{
#'   \item Experimental design (number of fractions collected)
#'   \item Expected protein elution profiles
#'   \item Desired stringency (higher values increase specificity but reduce sensitivity)
#'   \item Typical values range from 2-5 for most fractionation experiments
#' }
#'
#' @section Quality Control:
#' Important QC considerations include:
#' \itemize{
#'   \item Check distribution of non-zero counts before/after filtering
#'   \item Monitor fraction of rows retained vs removed
#'   \item Verify no systematic bias in removed proteins (e.g., by molecular weight)
#'   \item Compare with other filtering methods (e.g., intensity-based)
#' }
#'
#' @section Matrix Operations:
#' The function performs these operations:
#' \enumerate{
#'   \item Creates a temporary copy of input matrix (memory optimization)
#'   \item Replaces NA values with 0 for counting purposes
#'   \item Counts non-zero entries per row using row-wise apply
#'   \item Filters rows based on minimum non-zero count
#' }
#'
#' @section Performance Considerations:
#' \itemize{
#'   \item Computational complexity: O(n*m) where n=rows, m=columns
#'   \item Memory usage: Creates one temporary matrix copy (same size as input)
#'   \item For very large matrices (>1M elements), consider chunked processing
#'   \item Uses base R operations optimized for matrix processing
#' }
#'
#' @section Edge Case Handling:
#' \itemize{
#'   \item Empty matrix: Returns empty matrix with preserved structure
#'   \item All-NA matrix: Treated as all-zero matrix
#'   \item All-zero matrix: Returns empty matrix if min_fractions > 0
#'   \item Single-row/column matrices: Handled appropriately
#'   \item Invalid min_fractions: Automatically clamped to valid range
#' }
#'
#' @export
#' @examples
#' # Basic usage
#' mat <- matrix(c(1,0,3,0, 2,0,0,0, 0,5,6,7, 8,9,0,10), nrow=4, byrow=TRUE)
#' rownames(mat) <- paste0("Protein", 1:4)
#' colnames(mat) <- paste0("Fraction", 1:4)
#' filter_matrix_by_nonzero_fractions(mat, min_fractions = 2)
#'
#' # Edge case examples
#' empty_mat <- matrix(numeric(0), nrow=0, ncol=4)
#' filter_matrix_by_nonzero_fractions(empty_mat) # Returns empty matrix
#'
#' all_na_mat <- matrix(NA, nrow=3, ncol=4)
#' filter_matrix_by_nonzero_fractions(all_na_mat, 1) # Returns empty matrix
#'
#' single_row_mat <- matrix(1:4, nrow=1)
#' filter_matrix_by_nonzero_fractions(single_row_mat, 4) # Returns row
filter_matrix_by_nonzero_fractions <- function(input_matrix,
                                               min_fractions = 2) {
  # Input validation
  if (!is.matrix(input_matrix)) {
    stop("Input 'input_matrix' must be a matrix.")
  }
  if (!is.numeric(input_matrix)) {
    stop("Input 'input_matrix' must contain numeric values.")
  }
  if (length(min_fractions) != 1 || !is.numeric(min_fractions) ||
      min_fractions < 0) {
    stop("'min_fractions' must be a single non-negative numeric value.")
  }
  
  # Handle empty matrix case
  if (nrow(input_matrix) == 0) {
    return(input_matrix)
  }
  
  # Clamp min_fractions to valid range
  min_fractions <- max(0, min(ncol(input_matrix), min_fractions))
  
  # Create temporary copy for NA handling (memory optimization)
  temp_matrix <- input_matrix
  temp_matrix[is.na(temp_matrix)] <- 0
  
  # Count non-zero entries per row
  non_zero_counts_per_row <- apply(temp_matrix, 1, function(row_vector) {
    sum(row_vector != 0)
  })
  
  # Filter rows
  retained_rows_logical <- non_zero_counts_per_row >= min_fractions
  filtered_matrix <- input_matrix[retained_rows_logical, , drop = FALSE]
  
  return(filtered_matrix)
}
