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
#'
#' @return A character vector of protein IDs that pass the filter.
#' @importFrom progress progress_bar
#' @importFrom stats median mad
#' @export
RetainElutionProtein <- function(m, nmad, n_boot = 1000){
  m <-
    m + 1
  m[is.na(m)] <- 1
  mDat <-
    m[, seq_len(floor(ncol(m)/2)*2)]
  
  datDC <- matrix(0, nrow = nrow(mDat), ncol = n_boot)
  pb <- progress::progress_bar$new(
    format = "Calculating :what [:bar] :percent eta: :eta",
    clear = FALSE,
    total = n_boot, width = 80)
  
  max_len <- nchar(n_boot)
  for(i in seq_len(n_boot)){
    pb$tick(tokens = list(what = sprintf(paste0("%-", max_len, "s"), i)))

    idControl <-
      sample(seq_len(ncol(mDat)), ncol(mDat)/2)
    fqCtl <-
      rowSums((mDat[, idControl])^2)
    fqTgt <-
      rowSums((mDat[, -idControl])^2)
    datDC[, i] <-
      sqrt(fqCtl/fqTgt)
  }
  prtDC <-
    rowMeans(datDC)
  
  medDC <- stats::median(prtDC)
  madDC <- stats::mad(prtDC)
  
  prtPASS <-
    names(prtDC[(prtDC < medDC - nmad*madDC)|(prtDC > medDC + nmad*madDC)])
  return(prtPASS)
}

