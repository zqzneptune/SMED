#' Integrate Scores using a Single ML Model
#'
#' This is a lower-level integration function that trains a single ML model
#' using specified method `fnM`.
#'
#' @param rawScore A numeric matrix of elution-based scores (exclusive of PPI IDs).
#' @param rawResponse A character vector of "Y" and "N" labels for training.
#' @param fnM Character vector specifying the machine learning method.
#' @param seed Optional seed for reproducibility. Default is 100.
#'
#' @return A numeric vector of integrated scores.
#' @importFrom caret preProcess trainControl train twoClassSummary
#' @import data.table
#' @export
IntegrateScore <- function(rawScore, rawResponse, fnM, seed = 100){
  # RF Speed Hack
  modelMethod <- fnM
  if(fnM == "rf"){
    message("Optimization: Switching from 'rf' to 'ranger' for speed...")
    modelMethod <- "ranger"
  }
  
  message("Training model: ", modelMethod)
  
  # 1. Optimized Preprocessing
  preProModel <-
    caret::preProcess(rawScore, method = c("medianImpute", "center", "scale"))
  
  prepData <-
    predict(preProModel, newdata = rawScore)
  
  trainData <-
    data.frame(prepData)
  
  trainData[, "Response"] <-
    rawResponse
  
  # Filter training instances
  training <-
    trainData[!is.na(trainData$Response), , drop = FALSE]
  
  if (!is.null(seed)) set.seed(seed)
  fitControl <-
    caret::trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 1,
      savePredictions = "final",
      classProbs = TRUE,
      summaryFunction = caret::twoClassSummary,
      allowParallel = TRUE
    )
  
  if (!is.null(seed)) set.seed(seed)
  suppressMessages(models <-
                     caret::train(`Response` ~ .,
                           data = training,
                           method = modelMethod,
                           trControl = fitControl))
  
  message("Predicting...")
  preds <-
    predict(models, newdata = prepData, type = "prob")
  rm(models)
  gc()
  return(preds[, "Y"])
}