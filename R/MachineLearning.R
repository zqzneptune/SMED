#' Integrate Scores via Machine Learning
#'
#' This function integrates multiple elution-based scores using machine learning
#' models (passed via `fnMachine`) to produce a final integrated PPI score.
#'
#' @param rawScore A data frame containing PPI pairs and their elution-based scores.
#' @param refIntTrain A list containing 'TP' and 'TN' data frames for training.
#' @param fnMachine Character vector of machine learning methods (e.g., "xgbTree", "rf").
#'
#' @return A data frame with the original scores and an additional 'SMED' column for the integrated score.
#' @importFrom caret trainControl train twoClassSummary
#' @importFrom parallel makePSOCKcluster stopCluster detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom matrixStats colMedians
#' @import data.table
#' @export
MachineLearning <- function(rawScore, refIntTrain, fnMachine){
  if (!inherits(rawScore, "data.table")) {
    rawScore <- data.table::as.data.table(rawScore)
  }
  
  rawScore[, PPI := paste(InteractorA, InteractorB, sep = "~")]
  rawPPI <- rawScore$PPI
  
  # Ensure datScore is numeric matrix for optimal performance
  datScore <- as.matrix(rawScore[, -c("InteractorA", "InteractorB", "PPI"), with = FALSE])
  
  message("Preprocessing data (imputation and scaling)...")
  col_meds <- matrixStats::colMedians(datScore, na.rm = TRUE)
  for(j in seq_len(ncol(datScore))){
    if(any(is.na(datScore[, j]))){
      datScore[is.na(datScore[, j]), j] <- col_meds[j]
    }
  }
  
  # scaling
  prepData <- as.data.frame(scale(datScore))
  
  # Handle zero-variance columns which become NaN after scaling
  for(j in seq_len(ncol(prepData))){
    if(any(is.nan(prepData[, j]))){
      prepData[, j] <- 0
    }
  }
  
  # Prepare training data
  trainData <-
    data.frame(`PPI` = rawPPI,
               prepData,
               stringsAsFactors = FALSE)
  
  trainData[, "Response"] <- factor(
    ifelse(trainData$PPI %in% refIntTrain$TP$PPI, "Y",
           ifelse(trainData$PPI %in% refIntTrain$TN$PPI, "N", NA)),
    levels = c("Y", "N")
  )
  
  training <-
    trainData[!is.na(trainData$Response), -1]
  
  # 2. Parallel Backend Setup
  # Using detectCores() - 1 for adaptive performance
  n_cores <- parallel::detectCores() - 1
  if (is.na(n_cores) || n_cores < 1) n_cores <- 1
  
  if (n_cores > 1) {
    message("Enabling parallel processing with ", n_cores, " cores...")
    cl <- parallel::makePSOCKcluster(n_cores)
    doParallel::registerDoParallel(cl)
    # Ensure cluster stops even if an error occurs
    on.exit(parallel::stopCluster(cl))
  }
  
  set.seed(100)
  fitControl <-
    caret::trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 10,
      savePredictions = "final",
      classProbs = TRUE,
      summaryFunction = caret::twoClassSummary,
      allowParallel = TRUE # Explicitly allow parallel execution
    )
  
  listPred <-
     list()
  for(fnM in fnMachine){
    # 3. RF Speed Hack: Switch "rf" to "ranger"
    modelMethod <- fnM
    if(fnM == "rf"){
      message("Optimization: Switching from 'rf' to 'ranger' for 5-10x speed boost...")
      modelMethod <- "ranger"
    }
    
    message("Training model: ", modelMethod)
    set.seed(100)
    
    # Capture possible warnings/errors in training
    models <- tryCatch({
      suppressMessages(caret::train(`Response` ~ .,
                                   data = training,
                                   method = modelMethod,
                                   trControl = fitControl))
    }, error = function(e) {
      message("Error training model '", modelMethod, "': ", e$message)
      return(NULL)
    })
    
    if(!is.null(models)){
      preds <-
        predict(models, newdata = prepData, type = "prob")
      listPred[[fnM]] <-
        preds[, "Y"]
      rm(models) # Memory cleanup
    }
  }
  
  if(length(listPred) == 0){
    stop("No models were successfully trained.")
  }
  
  datPred <- as.data.frame(do.call(cbind, listPred))
  
  avePred <-
    rowMeans(datPred)
  
  datScore <- cbind(rawScore, datPred)
  datScore[, SMED := avePred]
  
  return(datScore)
}
