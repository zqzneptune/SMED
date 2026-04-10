#' Integrate Scores using a Single ML Model
#'
#' This is a lower-level integration function that trains a single ML model
#' using specified method `fnM`.
#'
#' @param rawScore A numeric matrix of elution-based scores (exclusive of PPI IDs).
#' @param rawResponse A character vector of "Y" and "N" labels for training.
#' @param fnM Character vector specifying the machine learning method.
#'
#' @return A numeric vector of integrated scores.
#' @importFrom caret preProcess trainControl train twoClassSummary
#' @importFrom parallel makePSOCKcluster stopCluster detectCores
#' @importFrom doParallel registerDoParallel
#' @export
IntegrateScore <- function(rawScore, rawResponse, fnM){
  # RF Speed Hack
  modelMethod <- fnM
  if(fnM == "rf"){
    message("Optimization: Switching from 'rf' to 'ranger' for speed...")
    modelMethod <- "ranger"
  }
  
  message("Training model: ", modelMethod)
  
  # 1. Parallel Backend Setup (Adaptive)
  n_cores <- parallel::detectCores() - 1
  if (is.na(n_cores) || n_cores < 1) n_cores <- 1
  
  if (n_cores > 1) {
    cl <- parallel::makePSOCKcluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }
  
  # 2. Optimized Preprocessing
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
  
  set.seed(100)
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
  
  set.seed(100)
  suppressMessages(models <-
                     caret::train(`Response` ~ .,
                           data = training,
                           method = modelMethod,
                           trControl = fitControl))
  
  preds <-
    predict(models, newdata = prepData, type = "prob")
  rm(models)
  return(preds[, "Y"])
}