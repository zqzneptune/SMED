GetComplexPPI <- function(rawCpx){
  refCpx <-
    lapply(rawCpx, function(x){return(unique(x))})
  allGene <-
    unique(unlist(refCpx))
  allGene <-
    sort(allGene)
  allS <-
    RcppAlgos::comboGeneral(allGene, 2)
  datS <-
    data.frame(allS, stringsAsFactors = FALSE)
  colnames(datS) <-
    c("InteractorA", "InteractorB")
  datS[, "PPI"] <-
    paste(datS$InteractorA, datS$InteractorB, sep = "~")
  tpLst <-
    lapply(refCpx, function(prt){
      prt <-
        unique(prt)
      if(length(prt) > 1){
        s <-
          RcppAlgos::comboGeneral(prt, 2)
        d <-
          data.frame(s, stringsAsFactors = FALSE)
        colnames(d) <-
          c("InteractorA", "InteractorB")
        d[, "PPI"] <-
          paste(d$InteractorA, d$InteractorB, sep = "~")
        return(d)
      }else{
        return(NA)
      }
    })

  tpPPI <-
    unique(do.call(rbind, tpLst))
  rownames(tpPPI) <-
    NULL
  tpPPI <-
    tpPPI[!is.na(tpPPI$PPI), ]
  rnPPI <-
    datS[!(datS$PPI %in% tpPPI$PPI), ]
  return(list(`TP` = tpPPI, `TN` = rnPPI))
}
