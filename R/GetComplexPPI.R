#' Generate Training PPI Pairs from Protein Complexes
#'
#' This function takes a list of protein complexes and generates all possible
#' True Positive (TP) pairs within the complexes and True Negative (TN) pairs
#' (all other possible pairs among the proteins involved).
#'
#' @param rawCpx A list of character vectors, where each vector contains protein
#'   IDs belonging to a complex.
#'
#' @return A list with two elements:
#'   \item{TP}{A data frame of True Positive PPIs.}
#'   \item{TN}{A data frame of True Negative PPIs.}
#' @importFrom RcppAlgos comboGeneral
#' @import data.table
#' @export
GetComplexPPI <- function(rawCpx){
  refCpx <-
    lapply(rawCpx, function(x){return(unique(x))})
  allGene <-
    unique(unlist(refCpx))
  allGene <-
    sort(allGene)
  allS <-
    RcppAlgos::comboGeneral(allGene, 2)
  datS <- data.table::as.data.table(allS)
  colnames(datS) <- c("InteractorA", "InteractorB")
  datS[, PPI := paste(InteractorA, InteractorB, sep = "~")]
  tpLst <-
    lapply(refCpx, function(prt){
      prt <-
        unique(prt)
      if(length(prt) > 1){
        s <-
          RcppAlgos::comboGeneral(prt, 2)
        d <- data.table::as.data.table(s)
        colnames(d) <- c("InteractorA", "InteractorB")
        d[, PPI := paste(InteractorA, InteractorB, sep = "~")]
        return(d)
      }else{
        return(NULL)
      }
    })

  tpPPI <- unique(data.table::rbindlist(tpLst, use.names = TRUE))
  rownames(tpPPI) <-
    NULL
  tpPPI <-
    tpPPI[!is.na(tpPPI$PPI), ]
  rnPPI <-
    datS[!(datS$PPI %in% tpPPI$PPI), ]
  return(list(`TP` = tpPPI, `TN` = rnPPI))
}

