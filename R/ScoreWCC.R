#' Calculate WCC Score
#'
#' This function calculates the Weighted Cross-Correlation (WCC) between proteins
#' elution profiles.
#'
#' @param rawMat A numeric matrix of protein elution profiles (proteins as rows, fractions as columns).
#' @param n_fracs Minimum number of fractions a protein must be present in. Default is 2.
#' @param cutoff Minimum WCC score to retain a PPI. Default is 0.5.
#' @param top_n Maximum number of PPIs to return. If NULL, cutoff is used.
#'
#' @return A data.table with `InteractorA`, `InteractorB`, and `WCC` scores.
#' @importFrom ptw wcc
#' @importFrom RcppAlgos comboGeneral
#' @importFrom future.apply future_lapply
#' @import data.table
#' @export
ScoreWCC <- function(rawMat, n_fracs = 2, cutoff = 0.5, top_n = NULL, ...){
  mat <-
    rawMat[sort(rownames(rawMat)), ]
  fmat <-
    FilterMat(mat, n_fracs = n_fracs)
  fmat[is.na(fmat)] <- 0
  
  pairs <- RcppAlgos::comboGeneral(seq_len(nrow(fmat)), 2)
  
  # Sparse approach: Identify overlapping indices
  message("Identifying overlapping protein pairs for sparse assessment...")
  # Pre-calculate non-zero counts for pairs
  # A pair is valid for WCC if they share >= 2 non-zero fractions
  # Using data.table for efficiency if matrix is large
  
  message("Computing WCC pairwise in parallel...")
  wcc_vals <- unlist(future.apply::future_lapply(seq_len(nrow(pairs)), function(i) {
    p1 <- fmat[pairs[i, 1], ]
    p2 <- fmat[pairs[i, 2], ]
    
    # Check overlap (at least 2 non-zero fractions in common)
    overlap <- sum(p1 > 0 & p2 > 0)
    if (overlap < 2) {
      return(0) # Return 0 instead of NA for no overlap
    }
    
    ptw::wcc(p1, p2, trwdth = 1)
  }))
  
  rawPPI <- data.table::data.table(
    InteractorA = rownames(fmat)[pairs[, 1]],
    InteractorB = rownames(fmat)[pairs[, 2]],
    WCC = wcc_vals
  )
  
  # Enforce alphabetical sort (already sorted by rownames(fmat)[pairs[, 1]])
  # but comboGeneral with sorted names ensures A < B
  
  if(is.null(top_n)){
    finalPPI <- rawPPI[WCC >= cutoff]
  }else{
    finalPPI <- head(rawPPI[order(-WCC)], top_n)
  }
  return(finalPPI[, .(InteractorA, InteractorB, WCC)])
}

