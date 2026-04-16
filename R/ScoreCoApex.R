#' Calculate CoApex Score
#'
#' This function calculates the CoApex score between proteins based on identifying
#' the fraction (apex) where each protein reaches its maximum intensity.
#'
#' @param rawMat A numeric matrix of protein elution profiles (proteins as rows, fractions as columns).
#' @param n_fracs Minimum number of fractions a protein must be present in. Default is 0.
#' @param top_ppi Maximum number of PPIs to return. Default is 20000.
#'
#' @return A data.table with `InteractorA`, `InteractorB`, and `normCoApex` scores.
#' @import data.table
#' @export
ScoreCoApex <- function(rawMat, n_fracs = 0, top_ppi = 20000, ...){
  if(!is.matrix(rawMat)){
    stop("rawMat must be a matrix.")
  }
  
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
  
  # Identify proteins with valid apex
  valid_proteins <- names(apexPrt)[!is.na(apexPrt)]
  
  rawPPI <- GetPrtPPI(rownames(fmat))
  
  # Initialize scores with 0
  rawPPI[, normCoApex := 0.0]
  
  if(length(valid_proteins) >= 2){
    apex_subset <- apexPrt[valid_proteins]
    pairwise_diff <- abs(outer(apex_subset, apex_subset, FUN = "-"))
    
    # Map back to rawPPI
    # This is slightly complex because GetPrtPPI returns all pairs
    # We only update pairs where both proteins have a valid apex
    
    # For efficiency with large data, we could use a different approach
    # but for CoApex (distance based) this is standard.
    
    # Construct a lookup table for the pairs that have valid apexes
    valid_pairs_dt <- GetPrtPPI(valid_proteins)
    
    # Fill in the absolute differences
    # pairwise_diff is a matrix with rownames/colnames = valid_proteins
    # We want the values for pairs in valid_pairs_dt
    
    # Subsetting matrix with matrix of indices/names
    idx_matrix <- as.matrix(valid_pairs_dt[, .(InteractorA, InteractorB)])
    valid_diffs <- pairwise_diff[idx_matrix]
    
    # Normalize diffs: (max - diff) / (max - min) -> 1 is best (0 diff), 0 is worst (max diff)
    diff_max <- max(valid_diffs, na.rm = TRUE)
    diff_min <- min(valid_diffs, na.rm = TRUE)
    diff_range <- diff_max - diff_min
    
    if(diff_range == 0){
      valid_norm <- rep(1.0, length(valid_diffs))
    } else {
      valid_norm <- (diff_max - valid_diffs) / diff_range
    }
    
    valid_pairs_dt[, normCoApex := valid_norm]
    
    # Merge back to rawPPI
    rawPPI[valid_pairs_dt, on = .(InteractorA, InteractorB), normCoApex := i.normCoApex]
  }

  finalPPI <- rawPPI[order(-normCoApex)]
  
  if(nrow(finalPPI) > top_ppi){
    datPPI <- finalPPI[1:top_ppi, ]
  }else{
    datPPI <- finalPPI
  }
  return(datPPI[, .(InteractorA, InteractorB, normCoApex)])
}


