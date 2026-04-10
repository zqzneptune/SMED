#' Aggregate Elution-based PPI Scores
#'
#' This function runs selected scoring methods (from CoApex, DICE, MI, PCCN, WCC)
#' on a raw elution matrix and merges the results into a single data frame.
#'
#' @param mRaw A numeric matrix of protein elution profiles (proteins as rows, fractions as columns).
#' @param n_fracs Minimum number of fractions a protein must be present in. Default is 2.
#' @param top_ppi Maximum number of PPIs to return for each method (if applicable). Default is 20000.
#' @param methods Character vector specifying which scoring methods to include. 
#'   Available options: "CoApex", "DICE", "MI", "PCCN", "WCC". 
#'   Default includes all five.
#'
#' @return A data frame containing the aggregated scores for each PPI.
#' @examples
#' data(dummy_elution_matrix)
#' # Calculate only MI and PCCN scores
#' scores <- ElutionScore(dummy_elution_matrix, n_fracs = 1, top_ppi = 100, 
#'                        methods = c("MI", "PCCN"))
#' head(scores)
#' @import data.table
#' @export
ElutionScore <- function(mRaw, n_fracs = 2, top_ppi = 20000, 
                        methods = c("CoApex", "DICE", "MI", "PCCN", "WCC")) {
  
  methods <- match.arg(methods, c("CoApex", "DICE", "MI", "PCCN", "WCC"), several.ok = TRUE)
  
  listScores <- list()
  
  if ("CoApex" %in% methods) {
    message("Calculating CoApex score...")
    listScores[["CoApex"]] <- ScoreCoApex(mRaw, n_fracs = n_fracs, top_ppi = top_ppi)
  }
  
  if ("DICE" %in% methods) {
    message("Calculating DICE score...")
    listScores[["DICE"]] <- ScoreDICE(mRaw, n_fracs = n_fracs, top_ppi = top_ppi)
  }
  
  if ("MI" %in% methods) {
    message("Calculating MI score...")
    listScores[["MI"]] <- ScoreMI(mRaw, n_fracs = n_fracs, top_n = top_ppi)
  }
  
  if ("PCCN" %in% methods) {
    message("Calculating PCCN score...")
    listScores[["PCCN"]] <- ScorePCCN(mRaw, n_fracs = n_fracs, top_n = top_ppi)
  }
  
  if ("WCC" %in% methods) {
    message("Calculating WCC score...")
    listScores[["WCC"]] <- ScoreWCC(mRaw, n_fracs = n_fracs, top_n = top_ppi)
  }
  
  if (length(listScores) == 0) {
    stop("No valid scoring methods selected.")
  }
  
  message("Merging scores into a single data.table...")
  
  # Ensure all elements in listScores are data.tables (should be by default, but safe to cast)
  listScores <- lapply(listScores, data.table::as.data.table)
  
  # Reduce with data.table merge to integrate all results efficiently
  res <- Reduce(
    function(dt1, dt2) merge(dt1, dt2, by = c("InteractorA", "InteractorB"), all = TRUE), 
    listScores
  )
  
  return(res)
}


