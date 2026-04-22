#' Calculate DICE Score
#'
#' This function calculates the DICE coefficient between proteins based on their
#' presence/absence across fractionation.
#'
#' @param rawMat A numeric matrix of protein elution profiles (proteins as rows, fractions as columns).
#' @param n_fracs Minimum number of fractions a protein must be present in. Default is 0.
#' @param top_ppi Maximum number of PPIs to return. Default is 20000.
#' @param ... Additional arguments (not currently used).
#'
#' @return A data.table with `InteractorA`, `InteractorB`, and `DICE` scores.
#' @importFrom arules dissimilarity
#' @import data.table
#' @export
ScoreDICE <- function(rawMat, n_fracs = 0, top_ppi = 20000, ...){
  mat <-
    rawMat[sort(rownames(rawMat)), ]
  fmat <-
    FilterMat(mat, n_fracs = n_fracs)
  fmat[is.na(fmat)] <- 0
  
  # Binary matrix for DICE calculation
  binMat <- (fmat > 0)
  
  # Identify proteins
  prts <- rownames(fmat)
  
  diceMat <-
    as.matrix(arules::dissimilarity(binMat, method = "dice"))
  colnames(diceMat) <- rownames(diceMat) <- prts
  
  # GetPrtPPI returns sorted names
  rawPPI <-
    GetPrtPPI(prts)
    
  # Use explicit matching instead of lower.tri to be robust
  idxA <- match(rawPPI$InteractorA, prts)
  idxB <- match(rawPPI$InteractorB, prts)
  rawPPI[, DICE := diceMat[cbind(idxA, idxB)]]
  
  # arules::dissimilarity produces distance (1 - dice similarity)
  rawPPI[, DICE := 1 - DICE]
  
  finalPPI <- rawPPI[order(-DICE)]
  
  if(nrow(finalPPI) > top_ppi){
    datPPI <- finalPPI[1:top_ppi, ]
  }else{
    datPPI <- finalPPI
  }
  return(datPPI[, .(InteractorA, InteractorB, DICE)])
}

