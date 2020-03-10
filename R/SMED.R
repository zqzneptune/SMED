SMED <- function(datInput){
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
  P <- 100
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
    names(prtDC[(prtDC < median(prtDC) - 2*mad(prtDC))|(prtDC > median(prtDC) + 2*mad(prtDC))])
  
  datCnt <- 
    datInput %>% 
    filter(`idPrey` %in% prtPASS) %>% 
    mutate(`NormalSpec` = `countPrey`/`lenPrey`) %>% 
    group_by(`idRun`) %>% 
    mutate(`SumNS` = sum(`NormalSpec`)) %>% 
    mutate(`NSAF` = `NormalSpec`/`SumNS`) %>% 
    group_by(`idRun`) %>% 
    mutate(`NormalNSAF` = `NSAF`/min(`NSAF`)) %>% 
    mutate(`Tn` = as.integer(sqrt(`NormalNSAF`)))
  
  d <- tidyr::spread(datCnt[, c("idRun", "idPrey", "Tn")], 
                     `idRun`, `Tn`)
  pps <-
    RcppAlgos::comboGeneral(d$idPrey, 2)
  
  f <-
    t(as.matrix(d[, -1]))
  f[!is.na(f)] <- 1
  f[is.na(f)] <- 0
  
  DICE <- .GetDICE(f)
  
  
  g <- 
    t(as.matrix(d[, -1]))
  g[is.na(g)] <- 0
  
  
  MinTN <- .GetMinTN(g)
  rawPPI <- 
    data.frame(pps, 
               DICE[lower.tri(DICE, diag = FALSE)],
               MinTN[lower.tri(MinTN, diag = FALSE)],
               stringsAsFactors = FALSE)
  colnames(rawPPI) <- 
    c("InteractorA", "InteractorB", "DICE", "ppiTN")
  datPPI <-
    rawPPI[rawPPI$ppiTN != 0, ]
  
  datPPI$ppiTN <- 
    as.numeric(datPPI$ppiTN)
  tnInteractorA <- 
    datPPI[, c("InteractorA", "ppiTN")]
  colnames(tnInteractorA) <- 
    c("UniprotID", "ppiTN")
  tnInteractorB <- 
    datPPI[, c("InteractorB", "ppiTN")]
  colnames(tnInteractorB) <- 
    c("UniprotID", "ppiTN")
  tnProtein <- 
    bind_rows(tnInteractorA, tnInteractorB) %>% 
    group_by(`UniprotID`) %>% 
    summarise(minTn = sum(`ppiTN`))
  sumMinTnInteractorA <- 
    tnProtein
  colnames(sumMinTnInteractorA) <- 
    c("InteractorA", "tnA")
  sumMinTnInteractorB <- 
    tnProtein
  colnames(sumMinTnInteractorB) <- 
    c("InteractorB", "tnB")
  scorePPI <- 
    datPPI %>% 
    left_join(., sumMinTnInteractorA, by = "InteractorA") %>% 
    left_join(., sumMinTnInteractorB, by = "InteractorB") %>% 
    mutate(`NMinTn` = sum(tnProtein$minTn)/2) %>% 
    mutate(`rawHyper` = -phyper(`ppiTN`, `tnA`, `NMinTn` - `tnB`, 
                                `tnB`, lower.tail = FALSE, log.p = TRUE)) %>% 
    mutate(`SMED` = `DICE` * `rawHyper`) %>% 
    mutate(`PPI` = paste(`InteractorA`, `InteractorB`, sep = "~"))
  return(scorePPI)
  
}
