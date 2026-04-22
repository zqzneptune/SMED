#' Integrate Scores via Machine Learning
#'
#' This function integrates multiple elution-based scores using machine learning
#' models (passed via `fnMachine`) to produce a final integrated PPI score.
#'
#' @param rawScore A data frame containing PPI pairs and their elution-based scores.
#' @param refIntTrain A list containing 'TP' and 'TN' data frames for training.
#' @param fnMachine Character vector of machine learning methods (e.g., "xgbTree", "rf").
#' @param seed Optional seed for reproducibility. Default is 100.
#' @param tuneLength Tuning length for caret::train. Default is 3.
#' @param ... Additional arguments passed to caret::train.
#'
#' @return A data.table with the original scores and an additional 'SMED' column for the integrated score.
#' @importFrom caret trainControl train twoClassSummary
#' @importFrom future.apply future_lapply
#' @importFrom matrixStats colMedians
#' @importFrom stats predict
#' @importFrom utils modifyList
#' @import data.table
#' @import ranger
#' @import xgboost
#' @export
MachineLearning <- function(rawScore, refIntTrain, fnMachine, seed = 100, tuneLength = 3, ...){
  if (!inherits(rawScore, "data.table")) {
    setDT(rawScore)
  }
  
  # Ensure InteractorA < InteractorB sorting and consistent PPI string
  norm_names <- .normalize_ppi(rawScore$InteractorA, rawScore$InteractorB)
  rawScore[, InteractorA := norm_names$A]
  rawScore[, InteractorB := norm_names$B]
  rawScore[, PPI := paste(InteractorA, InteractorB, sep = "~")]
  
  # Identify score columns
  score_cols <- setdiff(colnames(rawScore), c("InteractorA", "InteractorB", "PPI"))
  
  # Implement "Missingness Flag" feature
  # Note: These are now computed BEFORE splitting, which is acceptable for 
  # structural missingness in co-elution, but we keep it simple.
  message("Adding Missingness Flags...")
  for(col in score_cols){
    flag_name <- paste0("is_na_", col)
    rawScore[, (flag_name) := as.numeric(is.na(get(col)))]
  }
  
  all_feature_cols <- setdiff(colnames(rawScore), c("InteractorA", "InteractorB", "PPI"))
  
  # Imputation: data.table based update-in-place
  # We do this before training to avoid "missing values in object" errors in some models
  message("Preprocessing data (imputation)...")
  for(col in score_cols){
    med_val <- median(rawScore[[col]], na.rm = TRUE)
    if(is.na(med_val)) med_val <- 0 # Fallback if whole column is NA
    set(rawScore, i = which(is.na(rawScore[[col]])), j = col, value = med_val)
  }
  
  # Prepare training data with normalized PPI strings for matching
  train_ppi_tp <- .get_ppi_string(refIntTrain$TP$InteractorA, refIntTrain$TP$InteractorB)
  train_ppi_tn <- .get_ppi_string(refIntTrain$TN$InteractorA, refIntTrain$TN$InteractorB)
  
  rawScore[, Response := factor(
    ifelse(PPI %in% train_ppi_tp, "Y",
           ifelse(PPI %in% train_ppi_tn, "N", NA)),
    levels = c("Y", "N")
  )]
  
  training <- rawScore[!is.na(Response)]
  
  if (nrow(training[Response == "Y"]) < 2 || nrow(training[Response == "N"]) < 2) {
    stop("Training requires at least 2 TP and 2 TN examples.")
  }

  fitControl <-
    caret::trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 3,
      savePredictions = "final",
      classProbs = TRUE,
      summaryFunction = caret::twoClassSummary,
      allowParallel = TRUE
    )
  
  listPred <- list()
  for(fnM in fnMachine){
    extra_args <- list(...)
    modelMethod <- fnM # Default
    
    if(fnM == "rf"){
      message("Optimization: Using 'ranger' for Random Forest...")
      modelMethod <- "ranger"
      if(is.null(extra_args$num.threads)){
        extra_args$num.threads <- .get_n_cores()
      }
    }
    
    message("Training model: ", modelMethod)
    if (!is.null(seed)) set.seed(seed)
    
    # Filter out non-caret parameters
    scoring_params <- c("n_fracs", "top_ppi", "top_n", "methods", "cutoff")
    ml_args <- extra_args[!names(extra_args) %in% scoring_params]

    train_args <- list(
      form = Response ~ .,
      data = as.data.frame(training[, c("Response", all_feature_cols), with = FALSE]),
      method = modelMethod,
      metric = "ROC",
      trControl = fitControl,
      tuneLength = tuneLength,
      # Unified preprocessing inside caret
      preProcess = c("nzv", "center", "scale")
    )
    train_args <- modifyList(train_args, ml_args)
    
    model_fit <- tryCatch({
       do.call(caret::train, train_args)
    }, error = function(e) {
      message("Error training model '", modelMethod, "': ", e$message)
      return(NULL)
    })
    
    if(!is.null(model_fit)){
      message("Making predictions for ", modelMethod, "...")
      preds <- predict(model_fit, newdata = rawScore, type = "prob")
      listPred[[fnM]] <- preds[, "Y"]
    }
    gc()
  }
  
  if(length(listPred) == 0){
    stop("No models were successfully trained.")
  }
  
  datPred <- as.data.table(listPred)
  rawScore[, (fnMachine) := datPred]
  rawScore[, SMED := rowMeans(datPred)]
  
  return(rawScore)
}
