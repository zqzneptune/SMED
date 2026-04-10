#' Calculate WCC Score
#'
#' This function calculates the Weighted Cross-Correlation (WCC) between proteins
#' elution profiles.
#'
#' @param rawMat A numeric matrix of protein elution profiles (proteins as rows, fractions as columns).
#' @param n_fracs Minimum number of fractions a protein must be present in. Default is 2.
#' @param cutoff Minimum WCC score to retain a PPI. Default is 0.5.
#' @param top_n Maximum number of PPIs to return. If NULL, cutoff is used.
#' @param n_cores Number of cores for parallel execution. Default is max(1, parallel::detectCores() - 1).
#'
#' @return A data.table with `InteractorA`, `InteractorB`, and `WCC` scores.
#' @importFrom ptw wcc
#' @importFrom RcppAlgos comboGeneral
#' @importFrom parallel mclapply detectCores
#' @import data.table
#' @export
ScoreWCC <- function(rawMat, n_fracs = 2, cutoff = 0.5, top_n = NULL, n_cores = max(1, parallel::detectCores() - 1)){
  mat <-
    rawMat[sort(rownames(rawMat)), ]
  fmat <-
    FilterMat(mat, n_fracs = n_fracs)
  fmat[is.na(fmat)] <- 0
  
  pairs <- RcppAlgos::comboGeneral(seq_len(nrow(fmat)), 2)
  
  message(sprintf("Computing WCC pairwise sequentially over %d cores...", n_cores))
  wcc_vals <- unlist(parallel::mclapply(seq_len(nrow(pairs)), function(i) {
    ptw::wcc(fmat[pairs[i, 1], ], fmat[pairs[i, 2], ], trwdth = 1)
  }, mc.cores = n_cores))
  
  rawPPI <- data.table::data.table(
    InteractorA = rownames(fmat)[pairs[, 1]],
    InteractorB = rownames(fmat)[pairs[, 2]],
    WCC = wcc_vals
  )
  if(is.null(top_n)){
    finalPPI <- rawPPI[WCC >= cutoff]
  }else{
    finalPPI <- head(rawPPI[order(-WCC)], top_n)
  }
  return(finalPPI[, .(InteractorA, InteractorB, WCC)])
}

