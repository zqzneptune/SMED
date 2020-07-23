RetainElutionProtein <- function(m, nmad){
  m <-
    m + 1
  m[is.na(m)] <- 1
  mDat <-
    m[, seq_len(floor(ncol(m)/2)*2)]
  library(progress)
  P <- 1000
  listDC <- list()
  pb <- progress_bar$new(
    format = "Calculating :what [:bar] :percent eta: :eta",
    clear = FALSE,
    total = P, width = 80)
  max_len <- max(nchar(P))
  for(i in c(1:P)){
    pb$tick(tokens = list(what = sprintf(paste0("%-", max_len, "s"), i)))

    idControl <-
      sample(seq_len(ncol(mDat)), ncol(mDat)/2)
    fqCtl <-
      rowSums((mDat[, idControl])^2)#log2(rowSums((mDat[, idControl])^2))
    fqTgt <-
      rowSums((mDat[, -idControl])^2)#log2(rowSums((mDat[, -idControl])^2))
    listDC[[i]] <-
      sqrt(fqCtl/fqTgt)#abs(fqCtl-fqTgt)
  }
  datDC <-
    do.call(cbind, listDC)
  prtDC <-
    rowMeans(datDC)
  prtPASS <-
    names(prtDC[(prtDC < median(prtDC) - nmad*mad(prtDC))|(prtDC > median(prtDC) + nmad*mad(prtDC))])
  return(prtPASS)
}
