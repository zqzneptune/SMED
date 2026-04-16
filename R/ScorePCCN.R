#' Calculate PCCN Score
#'
#' This function calculates the Poisson correlation coefficient network (PCCN)
#' score, which uses bootstrapping with Poisson distribution to estimate
#' correlation robustness.
#'
#' @param rawMat A numeric matrix of protein elution profiles (proteins as rows, fractions as columns).
#' @param n_fracs Minimum number of fractions a protein must be present in. Default is 2.
#' @param rept Number of bootstrap repetitions. Default is 10.
#' @param cutoff Minimum PCCN score to retain a PPI. Default is 0.5.
#' @param top_n Maximum number of PPIs to return. If NULL, cutoff is used.
#' @param seed Optional seed for reproducibility.
#'
#' @return A data.table with PPI pairs and their `PCCN` scores.
#' @importFrom stats rpois cor
#' @importFrom future.apply future_lapply
#' @importFrom progressr progressor
#' @import data.table
#' @export
ScorePCCN <- function(rawMat, n_fracs = 2, rept = 10, cutoff = 0.5, top_n = NULL, seed = NULL, ...){
  if (!is.null(seed)) set.seed(seed)
  
  mat <-
    rawMat[sort(rownames(rawMat)), ]
  fmat <-
    FilterMat(mat, n_fracs = n_fracs)
  fmat[is.na(fmat)] <- 0

  A <- fmat
  M <-
    ncol(A)
  N <-
    nrow(A)
    
  message("Compute PCCN in parallel...")
  
  # progressr integration
  p <- progressr::progressor(steps = rept)
  
  list_cor <- future.apply::future_lapply(seq_len(rept), function(i) {
    p()
    # Ensure each worker has a different seed if not using future's built-in seed handling
    # future_lapply handles seeds if future.seed = TRUE is passed (default)
    A.rpoisson <- matrix(stats::rpois(n = length(A), lambda = A), nrow = N, ncol = M)
    C.rpoisson <- A.rpoisson + 1/M
    B.rpoisson <- C.rpoisson/rowSums(C.rpoisson)
    B.cor <- suppressWarnings(stats::cor(t(B.rpoisson), use = "pairwise.complete.obs"))
    B.cor[is.na(B.cor)] <- 0
    return(B.cor)
  }, future.seed = TRUE)
  
  PCC.mat.avg <- Reduce("+", list_cor) / rept

  rawPPI <-
    GetPrtPPI(rownames(fmat))

  rawPPI[, PCCN := PCC.mat.avg[lower.tri(PCC.mat.avg, diag = FALSE)]]
  
  # Ensure InteractorA < InteractorB (GetPrtPPI usually does this but we reinforce)
  
  if(is.null(top_n)){
    finalPPI <- rawPPI[PCCN >= cutoff]
  }else{
    finalPPI <- head(rawPPI[order(-PCCN)], top_n)
  }

  return(finalPPI[, .(InteractorA, InteractorB, PCCN)])
}

