#' Filter Matrix Rows Based on Number of Non-Zero Entries
#'
#' Filters a numeric matrix, retaining rows that have at least a specified
#' number of non-zero (and non-NA) entries across its columns.
#' NAs are treated as zeros for the purpose of counting.
#'
#' @param input_matrix A numeric matrix.
#' @param min_fractions Integer, the minimum number of columns in which a row
#'   must have a non-zero value to be retained. Default is 2.
#'
#' @return A filtered matrix, containing only the rows that meet the
#'   `min_fractions` criterion. Row names are preserved.
#' @export
#' @examples
#' mat <- matrix(c(1,0,3,0, 2,0,0,0, 0,5,6,7, 8,9,0,10), nrow=4, byrow=TRUE)
#' rownames(mat) <- paste0("Protein", 1:4)
#' colnames(mat) <- paste0("Fraction", 1:4)
#' print("Original Matrix:")
#' print(mat)
#'
#' filtered_mat <- filter_matrix_by_nonzero_fractions(mat, min_fractions = 2)
#' print("Filtered Matrix (min_fractions = 2):")
#' print(filtered_mat)
#'
#' filtered_mat_strict <- filter_matrix_by_nonzero_fractions(mat, min_fractions = 3)
#' print("Filtered Matrix (min_fractions = 3):")
#' print(filtered_mat_strict)
filter_matrix_by_nonzero_fractions <- function(input_matrix,
                                               min_fractions = 2) {
  if (!is.matrix(input_matrix) || !is.numeric(input_matrix)) {
    stop("Input 'input_matrix' must be a numeric matrix.")
  }
  if (nrow(input_matrix) == 0) {
    return(input_matrix) # Return empty matrix if input is empty
  }

  # Create a copy to avoid modifying the original matrix if it's passed by ref
  temp_matrix <- input_matrix
  temp_matrix[is.na(temp_matrix)] <- 0 # Treat NAs as zeros for counting

  # Count non-zero entries per row
  non_zero_counts_per_row <- apply(temp_matrix, 1, function(row_vector) {
    sum(row_vector != 0)
  })

  # Keep rows where count is >= min_fractions
  retained_rows_logical <- non_zero_counts_per_row >= min_fractions
  filtered_matrix <- input_matrix[retained_rows_logical, , drop = FALSE]

  return(filtered_matrix)
}
