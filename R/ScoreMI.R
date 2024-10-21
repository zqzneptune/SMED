ScoreMI <- function(rawMat, n_fracs = 0, cutoff = 0.5, top_n = NULL){
  library(infotheo)
  mat <-
    rawMat[sort(rownames(rawMat)), ]

  fmat <-
    FilterMat(mat, n_fracs = n_fracs)

  fmat[is.na(fmat)] <- 0

  gmat <-
    discretize(t(fmat))

  miMat <-
    mutinformation(gmat, method = "emp")
  rawPPI <-
    GetPrtPPI(rownames(miMat))
  rawPPI[, "MI"] <-
    miMat[lower.tri(miMat, diag = FALSE)]
  if(is.null(top_n)){
    finalPPI <-
      rawPPI[rawPPI$MI >= cutoff, ]
  }else{
    finalPPI <-
      rawPPI[order(-rawPPI$MI), ][1:top_n, ]
  }
  return(finalPPI[, c("PPI", "MI")])
  # finalPPI <-
  #   rawPPI[rev(order(rawPPI$MI)), ]
  #
  # if(nrow(finalPPI) > top_ppi){
  #   datPPI <-
  #     finalPPI[1:top_ppi, ]
  # }else{
  #   datPPI <-
  #     finalPPI
  # }
  # return(datPPI[, c("PPI", "MI")])
}
