#' Normalize PPI Pairs
#' 
#' Internal utility to ensure InteractorA is alphabetically smaller than InteractorB.
#' 
#' @param A Character vector of first interactors.
#' @param B Character vector of second interactors.
#' @return A list with normalized `A` and `B`.
#' @keywords internal
.normalize_ppi <- function(A, B) {
  swap <- A > B
  resA <- A
  resB <- B
  resA[swap] <- B[swap]
  resB[swap] <- A[swap]
  return(list(A = resA, B = resB))
}

#' Get PPI String
#' 
#' Internal utility to create a normalized PPI string.
#' 
#' @param A Character vector of first interactors.
#' @param B Character vector of second interactors.
#' @return Character vector of "A~B" strings.
#' @keywords internal
.get_ppi_string <- function(A, B) {
  norm <- .normalize_ppi(A, B)
  paste(norm$A, norm$B, sep = "~")
}

#' Get Number of Cores for Parallelism
#' 
#' Internal utility to respect CRAN/Bioconductor and user settings.
#' 
#' @return Integer number of cores.
#' @keywords internal
.get_n_cores <- function() {
  # Respect BiocCheck and CRAN requirements: avoid detectCores() without checks
  # First check user option
  n_cores <- getOption("mc.cores", 2L)
  
  # Then check environment variable (e.g. on build systems)
  env_cores <- as.integer(Sys.getenv("R_PARALLELLY_AVAILABLECORES_FALLBACK", NA))
  if (!is.na(env_cores)) n_cores <- env_cores
  
  return(n_cores)
}
