PCCN <- function(mRaw, rept = 10){
  # d <-
  #   mRaw[rowSums(mRaw, na.rm = TRUE) != 0, ]
  # e <-
  #   d[sort(rownames(d)), ] ### Pre-sort to generate PPI
  #### Modified PCC Euclidiean ###
  # f <-
  #   e[apply(e, 1, function(x){
  #     return(sum(ifelse(x == 0, 1, 0))) # Remove one-hit wonder
  #   }) != ncol(e)-1, ]
  # A <-
  #   f[apply(f, 1, sd) != 0, ] # Remove SD=0
  A <- mRaw
  M <-
    ncol(A)
  N <-
    nrow(A)
  i <- 0
  # message("Compute PCC ...")
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
    B.cor[is.na(B.cor)] <-0
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
  PCC <-
    PCC.mat.avg[lower.tri(PCC.mat.avg, diag = FALSE)]
  # s <-
  #   RcppAlgos::comboGeneral(rownames(A), 2)
  # datPPI <-
  #   data.frame(`PPI` = paste(s[, 1], s[, 2], sep = "~"),
  #              `PCCN` = PCC,
  #              stringsAsFactors = FALSE)
  return(PCC)
}
