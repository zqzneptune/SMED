#' Identify High-Confidence Eluting Proteins
#'
#' This function identifies proteins that show non-random elution patterns
#' using a bootstrapping approach to compare protein intensities across
#' random fraction splits.
#'
#' @param m A numeric matrix of elution profiles.
#' @param nmad Number of Median Absolute Deviations (MAD) from the median to
#'   set the threshold.
#' @param n_boot Number of bootstrap repetitions. Default is 1000.
#' @param metric Function to calculate elution intensity (e.g., `sum`, `max`). 
#'   Default is `function(x) sum(x^2)`.
#' @param seed Optional seed for reproducibility.
#'
#' @return A character vector of protein IDs that pass the filter.
#' @importFrom progressr progressor
#' @importFrom stats median mad
#' @export
RetainElutionProtein <- function(m, nmad, n_boot = 1000, metric = function(x) sum(x^2), seed = NULL){
  if (!is.null(seed)) set.seed(seed)
  
  m <-
    m + 1
  m[is.na(m)] <- 1
  mDat <-
    m[, seq_len(floor(ncol(m)/2)*2)]
  
  datDC <- matrix(0, nrow = nrow(mDat), ncol = n_boot)
  
  p <- progressr::progressor(steps = n_boot)
  
  num_cols <- ncol(mDat)
  half_cols <- num_cols / 2
  
  for(i in seq_len(n_boot)){
    p(sprintf("Bootstrap %d", i))

    idControl <- sample(seq_len(num_cols), half_cols)
    
    # Apply metric row-wise
    fqCtl <- apply(mDat[, idControl, drop = FALSE], 1, metric)
    fqTgt <- apply(mDat[, -idControl, drop = FALSE], 1, metric)
    
    # Handle zeros to avoid NaN/Inf
    fqTgt[fqTgt == 0] <- 1e-10
    
    datDC[, i] <- sqrt(fqCtl/fqTgt)
  }
  
  prtDC <- rowMeans(datDC)
  
  medDC <- stats::median(prtDC)
  madDC <- stats::mad(prtDC)
  
  prtPASS <- names(prtDC[(prtDC < medDC - nmad*madDC) | (prtDC > medDC + nmad*madDC)])
  return(prtPASS)
}

