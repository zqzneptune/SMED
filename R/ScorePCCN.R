ScorePCCN <- function(rawMat, n_fracs = 2, rept = 10, cutoff = 0.5){
  mat <-
    rawMat[sort(rownames(rawMat)), ]
  fmat <-
    FilterMat(mat, n_fracs = n_fracs)
  fmat[is.na(fmat)] <- 0
  
  A <- fmat
  M <-
    ncol(A)
  N <-
    nrow(A)
  i <- 0
  message("Compute PCCN ...")
  pb <-
    txtProgressBar(min = 1, max = rept, style = 3)
  repeat{
    A.rpoisson <-
      apply(A, c(1, 2), function(x){rpois(1, lambda = x)})
    C.rpoisson <-
      A.rpoisson + 1/M
    B.rpoisson <-
      C.rpoisson/rowSums(C.rpoisson)
    i <- i + 1
    setTxtProgressBar(pb, i)
    B.cor <-
      suppressWarnings(cor(t(B.rpoisson), use = "pairwise.complete.obs"))
    B.cor[is.na(B.cor)] <- 0
    if(i == 1){
      PCC.mat <- B.cor
    }else{
      PCC.mat <- PCC.mat + B.cor
    }
    if(i == rept){
      break
    }
  }
  close(pb)
  
  PCC.mat.avg <-
    PCC.mat/rept
  
  rawPPI <-
    GetPrtPPI(rownames(fmat))
  
  rawPPI[, "PCCN"] <-
    PCC.mat.avg[lower.tri(PCC.mat.avg, diag = FALSE)]
  
  finalPPI <-
    rawPPI[rawPPI$PCCN >= cutoff, ]
  return(finalPPI[, c("PPI", "PCCN")])
  
  # finalPPI <-
  #   rawPPI[rev(order(rawPPI$PCCN)), ]
  # 
  # if(nrow(finalPPI) > top_ppi){
  #   datPPI <-
  #     finalPPI[1:top_ppi, ]
  # }else{
  #   datPPI <-
  #     finalPPI
  # }
  # return(datPPI[, c("PPI", "PCCN")])
}