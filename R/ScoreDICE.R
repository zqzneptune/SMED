#' Calculate DICE Score
#'
#' This function calculates the DICE coefficient between proteins based on their
#' presence/absence across fractionation.
#'
#' @param rawMat A numeric matrix of protein elution profiles (proteins as rows, fractions as columns).
#' @param n_fracs Minimum number of fractions a protein must be present in. Default is 0.
#' @param top_ppi Maximum number of PPIs to return. Default is 20000.
#'
#' @return A data frame with PPI pairs and their `DICE` scores.
#' @importFrom arules dissimilarity
#' @export
ScoreDICE <- function(rawMat, n_fracs = 0, top_ppi = 20000){
  mat <-
    rawMat[sort(rownames(rawMat)), ]
  fmat <-
    FilterMat(mat, n_fracs = n_fracs)
  fmat[is.na(fmat)] <- 0
  
  # Binary matrix for DICE calculation
  binMat <- (!is.na(fmat) & fmat > 0)
  
  diceMat <-
    as.matrix(arules::dissimilarity(binMat, method = "dice"))
  rawPPI <-
    GetPrtPPI(rownames(diceMat))
  rawPPI[, DICE := diceMat[lower.tri(diceMat, diag = FALSE)]]
  
  finalPPI <- rawPPI[order(-DICE)]
  
  if(nrow(finalPPI) > top_ppi){
    datPPI <- finalPPI[1:top_ppi, ]
  }else{
    datPPI <- finalPPI
  }
  return(datPPI[, .(InteractorA, InteractorB, DICE)])
}

