#' Calculate Mutual Information Score
#'
#' This function calculates the Mutual Information (MI) between proteins based
#' on their discretized elution profiles.
#'
#' @param rawMat A numeric matrix of protein elution profiles (proteins as rows, fractions as columns).
#' @param n_fracs Minimum number of fractions a protein must be present in. Default is 0.
#' @param cutoff Minimum MI score to retain a PPI. Default is 0.5.
#' @param top_n Maximum number of PPIs to return. If NULL, cutoff is used.
#' @param ... Additional arguments (not currently used).
#'
#' @return A data.table with `InteractorA`, `InteractorB`, and `MI` scores.
#' @importFrom infotheo discretize mutinformation
#' @importFrom utils head
#' @import data.table
#' @export
ScoreMI <- function(rawMat, n_fracs = 0, cutoff = 0.5, top_n = NULL, ...){
  mat <-
    rawMat[sort(rownames(rawMat)), ]

  fmat <-
    FilterMat(mat, n_fracs = n_fracs)

  fmat[is.na(fmat)] <- 0

  gmat <-
    infotheo::discretize(t(fmat))

  miMat <-
    infotheo::mutinformation(gmat, method = "emp")
    
  rawPPI <-
    GetPrtPPI(rownames(fmat))
    
  rawPPI[, MI := miMat[lower.tri(miMat, diag = FALSE)]]
  
  if(is.null(top_n)){
    finalPPI <- rawPPI[MI >= cutoff]
  }else{
    finalPPI <- head(rawPPI[order(-MI)], top_n)
  }
  return(finalPPI[, .(InteractorA, InteractorB, MI)])
}

