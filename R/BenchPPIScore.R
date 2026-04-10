#' Benchmark PPI Scores using ROC
#'
#' This function evaluates the performance of PPI scores against gold standard
#' True Positive (TP) and True Negative (TN) interactions using ROC analysis.
#'
#' @param ppi Character vector of PPI strings.
#' @param score Numeric vector of scores associated with `ppi`.
#' @param refInt A list with 'TP' and 'TN' data frames for benchmarking.
#'
#' @return A `pROC::roc` object.
#' @importFrom pROC roc
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @export
BenchPPIScore <- function(ppi, score, refInt){
  if(length(ppi) == length(score)){
    prt <- 
      unique(unlist(strsplit(ppi, "~")))
    ref <-
      lapply(refInt, function(raw){
        dat <-
          raw %>% 
            dplyr::filter((`InteractorA` %in% prt)&(`InteractorB` %in% prt))
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

