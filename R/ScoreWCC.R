ScoreWCC <- function(rawMat, n_fracs = 2, cutoff = 0.5){
  mat <-
    rawMat[sort(rownames(rawMat)), ]
  fmat <-
    FilterMat(mat, n_fracs = n_fracs)
  fmat[is.na(fmat)] <- 0
  wccMat <-
    as.matrix(proxy::dist(fmat, method = function(x,y){
      ptw::wcc(x, y, trwdth = 1)
    }))
  rawPPI <-
    GetPrtPPI(rownames(wccMat))
  rawPPI[, "WCC"] <-
    wccMat[lower.tri(wccMat, diag = FALSE)]

  finalPPI <-
    rawPPI[rawPPI$WCC >= cutoff, ]
  return(finalPPI[, c("PPI", "WCC")])
  # finalPPI <-
  #   rawPPI[rev(order(rawPPI$WCC)), ]
  # 
  # if(nrow(finalPPI) > top_ppi){
  #   datPPI <-
  #     finalPPI[1:top_ppi, ]
  # }else{
  #   datPPI <-
  #     finalPPI
  # }
  # return(datPPI[, c("PPI", "WCC")])
}
