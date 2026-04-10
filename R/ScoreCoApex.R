#' Calculate CoApex Score
#'
#' This function calculates the CoApex score between proteins based on identifying
#' the fraction (apex) where were each protein reaches its maximum intensity.
#'
#' @param rawMat A numeric matrix of protein elution profiles (proteins as rows, fractions as columns).
#' @param n_fracs Minimum number of fractions a protein must be present in. Default is 0.
#' @param top_ppi Maximum number of PPIs to return. Default is 20000.
#'
#' @return A data frame with PPI pairs and their `normCoApex` scores.
#' @export
ScoreCoApex <- function(rawMat, n_fracs = 0, top_ppi = 20000){
  if("matrix" %in% class(rawMat)){
    mat <-
      rawMat[sort(rownames(rawMat)), ]
    fmat <-
      FilterMat(mat, n_fracs = n_fracs)
    fmat[is.na(fmat)] <- 0
    # Only 1, or 2 occurrences apply
    first_peak <- max.col(fmat, ties.method = "first")
    last_peak <- max.col(fmat, ties.method = "last")
    apexPrt <- ifelse(last_peak - first_peak <= 2, (first_peak + last_peak) / 2, NA)
    names(apexPrt) <- rownames(fmat)
    apexPrt <-
      apexPrt[!is.na(apexPrt)]
    pairwise_diff <- 
      outer(apexPrt, apexPrt, FUN = "-")
    rawPPI <-
      GetPrtPPI(names(apexPrt))
    rawCoApex <-
      abs(pairwise_diff[lower.tri(pairwise_diff, diag = FALSE)])
    
    # Check for constant peaks to avoid division by zero
    diff_max_min <- max(rawCoApex) - min(rawCoApex)
    if(diff_max_min == 0){
      rawPPI[, normCoApex := 1]
    } else {
      rawPPI[, normCoApex := (max(rawCoApex)-rawCoApex)/diff_max_min]
    }

    finalPPI <- rawPPI[order(-normCoApex)]
    
    if(nrow(finalPPI) > top_ppi){
      datPPI <- finalPPI[1:top_ppi, ]
    }else{
      datPPI <- finalPPI
    }
    return(datPPI[, .(InteractorA, InteractorB, normCoApex)])
  }else{
    stop("Not matrix.")
  }
}


