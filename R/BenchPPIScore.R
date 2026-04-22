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
  if(length(ppi) != length(score)){
    stop("PPI and Score lengths don't match!")
  }
  
  # Normalize input PPIs
  # Split and re-normalize to be safe
  ppi_parts <- strsplit(ppi, "~")
  ppi_norm <- vapply(ppi_parts, function(p) {
    if(length(p) != 2) return(NA_character_)
    .get_ppi_string(p[1], p[2])
  }, character(1))
  
  # Normalize reference PPIs
  ref_tp <- .get_ppi_string(refInt$TP$InteractorA, refInt$TP$InteractorB)
  ref_tn <- .get_ppi_string(refInt$TN$InteractorA, refInt$TN$InteractorB)
  
  # Assign response
  respons <- ifelse(ppi_norm %in% ref_tp, 1, 
                    ifelse(ppi_norm %in% ref_tn, 0, NA))
  
  # Defensive check for ROC
  valid_idx <- !is.na(respons)
  if (sum(valid_idx) == 0) {
    stop("No PPIs found in reference sets.")
  }
  
  unique_labels <- unique(respons[valid_idx])
  if (length(unique_labels) < 2) {
    stop("ROC requires both TP and TN examples (found labels: ", 
         paste(unique_labels, collapse = ", "), ")")
  }
  
  fObjs <- pROC::roc(predictor = score, 
                     response = respons, 
                     na.rm = TRUE,
                     quiet = TRUE)
  return(fObjs)
}

