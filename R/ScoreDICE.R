ScoreDICE <- function(rawMat, n_fracs = 0, top_ppi = 20000){
  mat <-
    rawMat[sort(rownames(rawMat)), ]
  fmat <-
    FilterMat(mat, n_fracs = n_fracs)
  fmat[is.na(fmat)] <- 0
  diceMat <-
    as.matrix(arules::dissimilarity((!is.na(fmat) & fmat > 0), method = "dice"))
  rawPPI <-
    GetPrtPPI(rownames(diceMat))
  rawPPI[, "DICE"] <-
    diceMat[lower.tri(diceMat, diag = FALSE)]
  
  finalPPI <-
    rawPPI[rev(order(rawPPI$DICE)), ]
  
  if(nrow(finalPPI) > top_ppi){
    datPPI <-
      finalPPI[1:top_ppi, ]
  }else{
    datPPI <-
      finalPPI
  }
  return(datPPI[, c("PPI", "DICE")])
}
