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
#' @import data.table
#' @export
MachineLearning <- function(rawScore, refIntTrain, fnMachine, seed = 100, tuneLength = 3, ...){
  if (!inherits(rawScore, "data.table")) {
    setDT(rawScore)
  }
  
  # Ensure InteractorA < InteractorB sorting for consistency if not already done
  # But we assume scoring functions did this.
  
  rawScore[, PPI := paste(InteractorA, InteractorB, sep = "~")]
  rawPPI <- rawScore$PPI
  
  # Identify score columns
  score_cols <- setdiff(colnames(rawScore), c("InteractorA", "InteractorB", "PPI"))
  
  # Implement "Missingness Flag" feature
  message("Adding Missingness Flags...")
  for(col in score_cols){
    flag_name <- paste0("is_na_", col)
    rawScore[, (flag_name) := as.numeric(is.na(get(col)))]
  }
  
  # Updated score_cols to include flags for training, but imputation only for actual scores
  all_feature_cols <- setdiff(colnames(rawScore), c("InteractorA", "InteractorB", "PPI"))
  
  # Imputation: data.table based update-in-place
  message("Preprocessing data (imputation)...")
  for(col in score_cols){
    med_val <- median(rawScore[[col]], na.rm = TRUE)
    if(is.na(med_val)) med_val <- 0 # Fallback if whole column is NA
    set(rawScore, i = which(is.na(rawScore[[col]])), j = col, value = med_val)
  }
  
  # Create numeric matrix for scaling
  datScore <- as.matrix(rawScore[, all_feature_cols, with = FALSE])
  
  # Scaling
  message("Scaling features...")
  prepData <- scale(datScore)
  
  # Handle zero-variance columns which become NaN after scaling
  prepData[is.nan(prepData)] <- 0
  
  # Prepare training data
  # Use data.table for training data as well
  trainData <- data.table(PPI = rawPPI, as.data.table(prepData))
  
  trainData[, Response := factor(
    ifelse(PPI %in% refIntTrain$TP$PPI, "Y",
           ifelse(PPI %in% refIntTrain$TN$PPI, "N", NA)),
    levels = c("Y", "N")
  )]
  
  training <- trainData[!is.na(Response)]
  # Remove PPI column for training
  training_features <- training[, !c("PPI"), with = FALSE]
  
  # Parallel backend should be handled by the user via future::plan()
  # but caret uses foreach, which we can hook into.
  # future::plan(multisession) if not already set is a good idea in a wrapper,
  # but here we follow the instruction to use future.
  
  fitControl <-
    caret::trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 3, # Reduced from 10 for speed, standardizing
      savePredictions = "final",
      classProbs = TRUE,
      summaryFunction = caret::twoClassSummary,
      allowParallel = TRUE
    )
  
  listPred <- list()
  for(fnM in fnMachine){
    extra_args <- list(...)
    
    if(fnM == "rf"){
      message("Optimization: Using 'ranger' for Random Forest...")
      modelMethod <- "ranger"
      # Instruction: explicitly use num.threads inside ranger
      # We'll try to detect if we have a future plan or just use detectCores
      if(is.null(extra_args$num.threads)){
        n_cores <- parallel::detectCores() - 1
        if(is.na(n_cores) || n_cores < 1) n_cores <- 1
        extra_args$num.threads <- n_cores
      }
    }
    
    message("Training model: ", modelMethod)
    if (!is.null(seed)) set.seed(seed)
    
    # Filter out scoring-related parameters from extra_args to avoid warnings in caret/models
    # these are consumed by ElutionScore but currently passed through ...
    scoring_params <- c("n_fracs", "top_ppi", "top_n", "methods", "cutoff")
    ml_args <- extra_args[!names(extra_args) %in% scoring_params]

    # Merge dots and explicit params
    train_args <- list(
      form = Response ~ .,
      data = training_features,
      method = modelMethod,
      metric = "ROC", # Set metric to ROC to match twoClassSummary and avoid Accuracy warnings
      trControl = fitControl,
      tuneLength = tuneLength
    )
    train_args <- modifyList(train_args, ml_args)
    
    models <- tryCatch({
       do.call(caret::train, train_args)
    }, error = function(e) {
      message("Error training model '", modelMethod, "': ", e$message)
      return(NULL)
    })
    
    if(!is.null(models)){
      message("Making predictions for ", modelMethod, "...")
      preds <- predict(models, newdata = prepData, type = "prob")
      listPred[[fnM]] <- preds[, "Y"]
      rm(models)
    }
    gc() # Explicit garbage collection after each model as requested
  }
  
  if(length(listPred) == 0){
    stop("No models were successfully trained.")
  }
  
  datPred <- as.data.table(listPred)
  avePred <- rowMeans(datPred)
  
  # Combine results
  # We should return exactly what was requested: datScore with SMED column
  # Wait, the instruction says "Return exactly three columns: InteractorA, InteractorB, and the score" 
  # for score functions, but MachineLearning return is used by SMED.R which picks the columns.
  
  rawScore[, (fnMachine) := datPred]
  rawScore[, SMED := avePred]
  
  return(rawScore)
}
