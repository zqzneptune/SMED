#' SMED: Statistical Modelling of coelution mass spectrometry data
#'
#' Main function to infer protein-protein interactions from co-eluted mass spectrometry data.
#'
#' @param mRaw A numeric matrix of protein elution profiles (proteins as rows, fractions as columns).
#' @param trainInt A list containing TP (True Positive) and TN (True Negative) PPI pairs for training. Each should be a data frame with a 'PPI' column (e.g., "ProteinA~ProteinB").
#' @param fnMachine Character vector of machine learning methods to use (passed to caret::train). Default is "xgbTree" (XGBoost).
#' @param ... Additional parameters passed to \code{\link{ElutionScore}} (e.g., n_fracs, top_ppi).
#'
#' @return A data frame with columns: InteractorA, InteractorB, Score.
#' @examples
#' data(dummy_elution_matrix)
#' data(dummy_train_complexes)
#' # Run SMED with default settings
#' results <- SMED(dummy_elution_matrix, dummy_train_complexes, fnMachine = "rf", n_fracs = 1, top_ppi = 100)
#' head(results)
#' @import data.table
#' @export
SMED <- function(mRaw, trainInt, fnMachine = "xgbTree", ...){
  
  # 1. Calculate elution-based scores
  message("Identifying potential PPIs and calculating elution-based scores...")
  rawScore <- ElutionScore(mRaw, ...)
  
  # 2. Integrate scores via Machine Learning integration
  message("Integrating scores using Machine Learning models: ", paste(fnMachine, collapse = ", "))
  # MachineLearning logic is defined in R/MachineLearning.R
  datSMED <- MachineLearning(rawScore = rawScore,
                             refIntTrain = trainInt,
                             fnMachine = fnMachine)
  
  # 3. Format output as requested: InteractorA, InteractorB, Score
  message("Formatting final result as requested (InteractorA, InteractorB, Score)...")
  
  final_df <- datSMED[, .(InteractorA, InteractorB, Score = SMED)]
  
  # Return final formatted data frame
  return(final_df)
}

