ScoreCoApex <- function(rawMat, n_fracs = 0, top_ppi = 20000){
  if("matrix" %in% class(rawMat)){
    mat <-
      rawMat[sort(rownames(rawMat)), ]
    fmat <-
      FilterMat(mat, n_fracs = n_fracs)
    fmat[is.na(fmat)] <- 0
    # Only 1, or 2 occurrences apply
    apexPrt <-
      apply(fmat, 1, function(x){
        x[is.na(x)] <- 0
        valMax <-
          max(x, na.rm = TRUE)
        posMax <-
          seq_along(x)[x == valMax]
        if(length(posMax) == 1){
          return(posMax)
          
        }else if(length(posMax) == 2){
          if(posMax[2]-posMax[1] <= 2){
            return(mean(posMax))
          }else{
            return(NA)
          }
        }else{
          return(NA)
        }
      })
    apexPrt <-
      apexPrt[!is.na(apexPrt)]
    pairwise_diff <- 
      outer(apexPrt, apexPrt, FUN = "-")
    rawPPI <-
      GetPrtPPI(names(apexPrt))
    rawCoApex <-
      abs(pairwise_diff[lower.tri(pairwise_diff, diag = FALSE)])
    rawPPI[, "normCoApex"] <-
      (max(rawCoApex)-rawCoApex)/(max(rawCoApex) - min(rawCoApex))
    finalPPI <-
      rawPPI[rev(order(rawPPI$normCoApex)), ]
    
    if(nrow(finalPPI) > top_ppi){
      datPPI <-
        finalPPI[1:top_ppi, ]
    }else{
      datPPI <-
        finalPPI
    }
    return(datPPI[, c("PPI", "normCoApex")])
  }else{
    stop("Not matrix.")
  }
}

