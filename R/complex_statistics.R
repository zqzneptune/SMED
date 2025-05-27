#' Calculate Statistics for a List of Protein Complexes
#'
#' Computes various descriptive statistics for a given list of protein
#' complexes, such as the number of complexes, number of unique subunits,
#' average complex size, and percentages of specific N-mers.
#'
#' @param complex_list A list where each element is a character vector
#'   representing a protein complex (i.e., a list of protein identifiers).
#'
#' @return A named numeric vector with the following statistics:
#'   \item{num_clusters}{Total number of complexes in the list.}
#'   \item{num_subunits}{Total number of unique protein subunits across all
#'     complexes.}
#'   \item{pct_dimers}{Percentage of complexes that are dimers (size 2).}
#'   \item{pct_trimers}{Percentage of complexes that are trimers (size 3).}
#'   \item{pct_large_gt10}{Percentage of complexes with more than 10 subunits.}
#'   \item{avg_complex_size}{Average number of subunits per complex.}
#' @export
#' @examples
#' complexes <- list(
#'   cpx1 = c("A", "B"),
#'   cpx2 = c("B", "C", "D"),
#'   cpx3 = c("E", "F"),
#'   cpx4 = LETTERS[1:12]
#' )
#' stats <- calculate_complex_statistics(complexes)
#' print(stats)
calculate_complex_statistics <- function(complex_list) {
  if (!is.list(complex_list) ||
      any(sapply(complex_list, function(x) !is.character(x)))) {
    stop("Input 'complex_list' must be a list of character vectors.")
  }

  num_complexes <- length(complex_list)
  if (num_complexes == 0) {
    return(
      c(
        num_clusters = 0,
        num_subunits = 0,
        pct_dimers = 0,
        pct_trimers = 0,
        pct_large_gt10 = 0,
        avg_complex_size = NA_real_
      )
    )
  }

  subunit_counts_per_complex <- lengths(complex_list)
  sum_subunit_counts <- table(subunit_counts_per_complex)

  avg_complex_size <- mean(subunit_counts_per_complex, na.rm = TRUE)

  num_dimers <-
    sum_subunit_counts[names(sum_subunit_counts) == "2"]
  num_dimers <- if (length(num_dimers) == 0) 0 else unname(num_dimers)

  num_trimers <-
    sum_subunit_counts[names(sum_subunit_counts) == "3"]
  num_trimers <- if (length(num_trimers) == 0) 0 else unname(num_trimers)

  num_large_gt10 <-
    sum(sum_subunit_counts[
      as.numeric(names(sum_subunit_counts)) > 10
    ])

  unique_subunits <- unique(unlist(complex_list))

  results <- c(
    "num_clusters" = num_complexes,
    "num_subunits" = length(unique_subunits),
    "pct_dimers" = (num_dimers / num_complexes) * 100,
    "pct_trimers" = (num_trimers / num_complexes) * 100,
    "pct_large_gt10" = (num_large_gt10 / num_complexes) * 100,
    "avg_complex_size" = avg_complex_size
  )

  return(results)
}
