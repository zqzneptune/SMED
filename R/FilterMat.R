#' Filter Matrix by Fraction Presence
#'
#' This function filters an elution matrix to retain only proteins present in
#' at least `n_fracs` fractions.
#'
#' @param mat A numeric elution matrix (proteins as rows, fractions as columns).
#' @param n_fracs Minimum number of fractions a protein must be present in. Default is 2.
#'
#' @return The filtered numeric matrix.
#' @export
FilterMat <- function(mat, n_fracs = 2){
  mat[is.na(mat)] <- 0
  fracPrt <- rowSums(mat != 0, na.rm = TRUE)
  fMat <- mat[fracPrt >= n_fracs, , drop = FALSE]
  return(fMat)
}

