#' Generate Pairwise PPI Strings from Protein List
#'
#' This function generates all possible pairwise interactions (PPI strings)
#' from a list of protein IDs.
#'
#' @param prt A character vector of protein IDs.
#'
#' @return A data.table with columns `InteractorA` and `InteractorB`.
#' @importFrom RcppAlgos comboGeneral
#' @import data.table
#' @export
GetPrtPPI <- function(prt){
  prt <-
    sort(prt)
  s <-
    data.table::as.data.table(RcppAlgos::comboGeneral(prt, 2))
  colnames(s) <-
    c("InteractorA", "InteractorB")
  return(s)
}

