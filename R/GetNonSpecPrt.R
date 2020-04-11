GetNonSpecPrt <- function(datInput, nmad){
  # lsData <-
  #   listData[[1]]
  # d <-
  #   lsData[[1]]
  # d <-
  #   d[, !duplicated(colnames(d))]
  # datInput <-
  #   setNames(melt(d), c("gene", "fract", "counts")) %>%
  #   mutate(`gene` = as.character(`gene`),
  #          `fract` = as.character(`fract`)) %>%
  #   filter(`counts` != 0) %>%
  #   inner_join(., prtLen, by = c("gene" = "UniprotID"))
  # colnames(datInput) <-
  #   c("idPrey", "idRun", "countPrey", "lenPrey")
  #

  #### 1. Contanminant Removal ####
  # preyFeq <-
  #   datInput %>%
  #   group_by(`idPrey`) %>%
  #   summarise(`cntPrey` = n())
  # preyFeq[, "Feq"] <-
  #   preyFeq$cntPrey/length(unique(datInput$idRun))
  # hist(preyFeq$Feq, breaks = 100)

  mInput <-
    tidyr::spread(datInput[, c("idRun", "idPrey", "countPrey")],
                  `idRun`, `countPrey`)
  m <-
    as.matrix(mInput[, -1])
  rownames(m) <-
    mInput$idPrey
  m <-
    m + 1
  m[is.na(m)] <- 1
  dim(m)

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
