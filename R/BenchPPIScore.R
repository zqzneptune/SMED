BenchPPIScore <- function(ppi, score, refInt){
  if(length(ppi) == length(score)){
    prt <- 
      unique(unlist(strsplit(ppi, "~")))
    ref <-
      lapply(refInt, function(raw){
        dat <-
          raw %>% 
            filter((`InteractorA` %in% prt)&(`InteractorB` %in% prt))
        return(dat)
      })
    respons <-
      ifelse(ppi %in% ref$TP$PPI, 1, ifelse(ppi %in% ref$TN$PPI, 0, NA))
    
    fObjs <-
      pROC::roc(predictor = score, 
          response = respons, 
          na.rm = TRUE)
    return(fObjs)
  }else{
    stop("PPI and Score lengths don't match!")
  }
}
