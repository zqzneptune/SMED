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
#'
#' @return A data frame with PPI pairs and their `PCCN` scores.
#' @importFrom stats rpois cor
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
ScorePCCN <- function(rawMat, n_fracs = 2, rept = 10, cutoff = 0.5, top_n = NULL){
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
  i <- 0
  message("Compute PCCN ...")
  pb <-
    utils::txtProgressBar(min = 0, max = rept, style = 3)
  PCC.mat <- matrix(0, nrow = N, ncol = N)
  repeat{
    A.rpoisson <- matrix(stats::rpois(n = length(A), lambda = A), nrow = N, ncol = M)
    C.rpoisson <-
      A.rpoisson + 1/M
    B.rpoisson <-
      C.rpoisson/rowSums(C.rpoisson)
    i <- i + 1
    utils::setTxtProgressBar(pb, i)
    B.cor <-
      suppressWarnings(stats::cor(t(B.rpoisson), use = "pairwise.complete.obs"))
    B.cor[is.na(B.cor)] <- 0
    PCC.mat <- PCC.mat + B.cor
    if(i == rept){
      break
    }
  }
  close(pb)

  PCC.mat.avg <-
    PCC.mat/rept

  rawPPI <-
    GetPrtPPI(rownames(fmat))

  rawPPI[, PCCN := PCC.mat.avg[lower.tri(PCC.mat.avg, diag = FALSE)]]
  if(is.null(top_n)){
    finalPPI <- rawPPI[PCCN >= cutoff]
  }else{
    finalPPI <- head(rawPPI[order(-PCCN)], top_n)
  }

  return(finalPPI[, .(InteractorA, InteractorB, PCCN)])
}

