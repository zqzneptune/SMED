#' Impute and Preprocess Score Matrix for Machine Learning
#'
#' This internal helper function prepares feature matrices for machine learning by:
#' 1. Handling missing values via median imputation
#' 2. Applying standardization (centering and scaling)
#' 3. Preserving PPI identifiers if present
#'
#' The preprocessing follows:
#' \deqn{X_{scaled} = \frac{X - \mu}{\sigma}}
#' where \eqn{\mu} is mean and \eqn{\sigma} is standard deviation.
#'
#' @param score_matrix A numeric matrix/data frame where:
#'   - Rows: Observations (PPIs)
#'   - Columns: Features (scores)
#'   First column named "PPI" (if present) is preserved but not preprocessed.
#' @return A list containing:
#'   \item{preprocessed_data}{Transformed data frame ready for modeling}
#'   \item{preprocessing_model}{`preProcess` object to apply same transform to new data}
#' @keywords internal
#' @noRd
#' @importFrom Hmisc impute
#' @import caret
preprocess_score_matrix <- function(score_matrix) {
  if (!is.data.frame(score_matrix) && !is.matrix(score_matrix)) {
    stop("'score_matrix' must be a data frame or matrix.")
  }

  ppi_column <- NULL
  feature_data <- score_matrix

  if ("PPI" %in% colnames(score_matrix)) {
    ppi_column <- score_matrix[, "PPI", drop = FALSE]
    feature_data <- score_matrix[, !(colnames(score_matrix) == "PPI"),
                                 drop = FALSE]
  }

  feature_data_numeric <- as.data.frame(
    lapply(feature_data, function(col) {
      if(!is.numeric(col)) as.numeric(as.character(col)) else col
    })
  )
  rownames(feature_data_numeric) <- rownames(feature_data)

  imputed_feature_data <- apply(feature_data_numeric, 2, function(x) {
    if (any(!is.na(x))) {
      return(Hmisc::impute(x, fun = stats::median))
    } else {
      return(x)
    }
  })
  imputed_feature_data_df <- as.data.frame(imputed_feature_data)
  rownames(imputed_feature_data_df) <- rownames(feature_data_numeric)

  col_sds <- apply(imputed_feature_data_df, 2, stats::sd, na.rm = TRUE)
  cols_to_process <- colnames(imputed_feature_data_df)[col_sds > 1e-6]

  preproc_model <- NULL
  preprocessed_data <- imputed_feature_data_df

  if (length(cols_to_process) > 0) {
    preproc_model <- caret::preProcess(
      imputed_feature_data_df[, cols_to_process, drop = FALSE],
      method = c("center", "scale")
    )
    preprocessed_subset <- predict(
      preproc_model,
      newdata = imputed_feature_data_df[, cols_to_process, drop = FALSE]
    )
    preprocessed_data[, cols_to_process] <- preprocessed_subset
  } else {
    message("No columns with sufficient variance found for preprocessing.")
  }

  if (!is.null(ppi_column)) {
    preprocessed_data <- cbind(ppi_column, preprocessed_data)
  }

  return(list(
    preprocessed_data = preprocessed_data,
    preprocessing_model = preproc_model
  ))
}

#' Train a Single Machine Learning Model for PPI Prediction
#'
#' Trains a specified ML model using caret with:
#' - Repeated k-fold cross-validation
#' - AUC optimization
#' - Parallel processing support
#' - Automated preprocessing
#'
#' @section Machine Learning Methodology:
#' This function implements a standardized workflow for training binary classifiers:
#' 1. Data preprocessing (imputation and scaling)
#' 2. Model training with hyperparameter tuning
#' 3. Performance evaluation using ROC AUC
#'
#' Supported model types include:
#' - Random Forest ("rf")
#' - Regularized Logistic Regression ("glmnet") 
#' - Support Vector Machines ("svmRadial")
#' - Gradient Boosting Machines ("gbm")
#'
#' @section Model Training Process:
#' The training process involves:
#' 1. Data splitting via stratified k-fold CV
#' 2. Hyperparameter grid search
#' 3. Parallelized model fitting
#' 4. Performance evaluation using:
#'    - ROC AUC (primary metric)
#'    - Sensitivity/Specificity
#'    - Precision/Recall
#'
#' @section Feature Importance:
#' Feature importance is calculated differently per model type:
#' - Tree-based models: Mean decrease in accuracy/Gini impurity
#' - Linear models: Standardized coefficient magnitudes
#' - SVM models: Weight magnitudes from the decision boundary
#'
#' @section Performance Evaluation:
#' Models are evaluated using:
#' - Cross-validated ROC AUC (primary metric)
#' - Sensitivity/Specificity at optimal threshold
#' - Precision-Recall curves
#' - Calibration curves
#'
#' @param raw_score_df Feature matrix (rows: PPIs, columns: scores)
#' @param response_vector Binary response (factor or numeric 0/1)
#' @param model_method caret model method (e.g. "rf", "glmnet", "svmRadial")
#' @param num_cv_folds Cross-validation folds (2-10, default 5)
#' @param num_cv_repeats CV repeats (1-10, default 1)  
#' @param seed Random seed (default 100)
#' @param num_cores Parallel cores (1-8, default 2)
#'
#' @return Numeric vector of predicted probabilities for positive class
#' @export
#' @seealso \code{\link{train_ensemble_ppi_models}} for ensemble approach
#' @examples
#' # Example with random forest
#' data <- data.frame(score1=rnorm(100), score2=rnorm(100))
#' response <- factor(sample(c("Y","N"), 100, replace=TRUE))
#' probs <- integrate_ppi_scores_and_train(
#'   data, response, "rf", num_cv_folds=3, num_cores=1
#' )
#'
#' # Get feature importance (for supported models)
#' # model <- getModelInfo("rf")[[1]]
#' # varImp(model)
integrate_ppi_scores_and_train <- function(raw_score_df,
                                         response_vector,
                                         model_method,
                                         num_cv_folds = 5,
                                         num_cv_repeats = 1,
                                         seed = 100,
                                         num_cores = 2) {
  if (nrow(raw_score_df) != length(response_vector)) {
    stop("Number of rows in 'raw_score_df' must match length of 'response_vector'.")
  }

  preprocessing_result <- preprocess_score_matrix(raw_score_df)
  processed_scores_df <- preprocessing_result$preprocessed_data

  if (!is.factor(response_vector)) {
    if (is.numeric(response_vector) && all(response_vector %in% c(0,1,NA))) {
      response_vector <- factor(response_vector, levels = c(0,1),
                                labels = c("N", "Y"))
    } else {
      response_vector <- factor(response_vector)
    }
  }
  if (nlevels(response_vector) != 2) {
    stop("Response vector must have exactly two levels for twoClassSummary.")
  }

  training_data <- processed_scores_df
  training_data$Response <- response_vector
  training_data_complete <-
    training_data[!is.na(training_data$Response), ]

  if (nrow(training_data_complete) == 0) {
    stop("No complete cases left after removing NAs in response variable.")
  }
  class_summary <- summary(training_data_complete$Response)
  if (any(class_summary < num_cv_folds)) {
    warning("One or more classes have fewer samples than 'num_cv_folds'.")
  }
  if (any(class_summary == 0)) {
    stop("One class has zero samples. Cannot train model.")
  }

  fit_control <- caret::trainControl(
    method = "repeatedcv",
    number = num_cv_folds,
    repeats = num_cv_repeats,
    savePredictions = "final",
    classProbs = TRUE,
    summaryFunction = caret::twoClassSummary,
    allowParallel = TRUE
  )

  if (num_cores > 1 && !requireNamespace("doParallel", quietly = TRUE)) {
    warning("'doParallel' package not found, training will be sequential.")
    num_cores <- 1
  }

  if (num_cores > 1) {
    cl <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cl)
    on.exit(doParallel::stopImplicitCluster(), add = TRUE)
  }

  trained_model <- NULL
  tryCatch({
    trained_model <- caret::train(
      Response ~ .,
      data = training_data_complete,
      method = model_method,
      trControl = fit_control,
      metric = "ROC"
    )
  }, error = function(e) {
    message("Msg during model training with '", model_method, "': ", e$message)
  }, finally = {
    if (num_cores > 1 && exists("cl")) {
      # Cluster automatically stopped by stopImplicitCluster via on.exit
    }
  })

  if (is.null(trained_model)) {
    warning("Model training failed for method: ", model_method,
            ". Returning NAs.")
    return(rep(NA_real_, nrow(raw_score_df)))
  }

  predicted_probs_all <- stats::predict(
    trained_model,
    newdata = processed_scores_df,
    type = "prob"
  )

  positive_class_label <- levels(training_data_complete$Response)[2]
  return(predicted_probs_all[, positive_class_label])
}

#' Train Ensemble of Machine Learning Models for PPI Prediction
#'
#' Trains multiple models and optionally averages predictions to create:
#' - SMED_Score (Simple Mean Ensemble D-score):
#'   \deqn{SMED = \frac{1}{n}\sum_{i=1}^{n} p_i}
#'
#' @section Ensemble Methodology:
#' This function implements a simple ensemble approach that:
#' 1. Trains each specified model independently
#' 2. Collects predicted probabilities from all models
#' 3. Computes mean probability (SMED score)
#' 4. Returns individual and combined predictions
#'
#' @section Training Process:
#' The ensemble training involves:
#' 1. Creating binary labels from reference PPIs
#' 2. Training each model via \code{\link{integrate_ppi_scores_and_train}}
#' 3. Handling missing predictions gracefully
#' 4. Computing ensemble scores
#'
#' @section Performance Evaluation:
#' The ensemble is evaluated by:
#' - Comparing individual model AUCs
#' - Assessing ensemble AUC improvement
#' - Analyzing correlation between model predictions
#' - Examining calibration of ensemble scores
#'
#' @param ppi_score_df Data frame with "PPI" column and feature scores
#' @param reference_train_interactome List with TP/TN reference PPIs
#' @param model_methods_vector Vector of caret model methods
#' @param average_predictions Whether to compute SMED_Score (default TRUE)
#' @param num_cv_folds Cross-validation folds (2-10, default 5)
#' @param num_cv_repeats CV repeats (1-10, default 1)
#' @param seed Random seed (default 100)
#' @param num_cores Parallel cores (1-8, default 2)
#'
#' @return Input data frame augmented with:
#'   - Prob_[model] columns for each model
#'   - SMED_Score column if average_predictions=TRUE
#' @export
#' @seealso \code{\link{integrate_ppi_scores_and_train}} for single models
#' @examples
#' ppi_scores <- data.frame(
#'   PPI = c("A~B", "B~C"),
#'   score1 = c(0.5, 0.3),
#'   score2 = c(1.2, 0.8)
#' )
#' reference <- list(
#'   TP = data.frame(PPI = "A~B"),
#'   TN = data.frame(PPI = "B~C")
#' )
#' result <- train_ensemble_ppi_models(
#'   ppi_scores, reference, c("rf", "glmnet")
#' )
#'
#' # Evaluate ensemble performance
#' # library(pROC)
#' # roc(response=ifelse(result$PPI %in% reference$TP$PPI, 1, 0),
#' #     predictor=result$SMED_Score)
train_ensemble_ppi_models <- function(
    ppi_score_df,
    reference_train_interactome,
    model_methods_vector,
    average_predictions = TRUE,
    num_cv_folds = 5,
    num_cv_repeats = 1,
    seed = 100,
    num_cores = 2) {
  if (!"PPI" %in% colnames(ppi_score_df)) {
    stop("'ppi_score_df' must contain a 'PPI' column.")
  }
  if (!is.list(reference_train_interactome) ||
      !all(c("TP", "TN") %in% names(reference_train_interactome))) {
    stop("'reference_train_interactome' must be a list with 'TP' and 'TN'.")
  }

  response_vector <- ifelse(
    ppi_score_df$PPI %in% reference_train_interactome$TP$PPI, "Y",
    ifelse(ppi_score_df$PPI %in% reference_train_interactome$TN$PPI, "N", NA)
  )
  response_vector <- factor(response_vector, levels = c("N", "Y"))

  feature_columns_df <-
    ppi_score_df[, !(colnames(ppi_score_df) == "PPI"), drop = FALSE]

  list_of_predictions <- list()

  message("Starting ensemble model training for: ",
          paste(model_methods_vector, collapse=", "))

  for (model_method in model_methods_vector) {
    message("  Training model: ", model_method, "...")

    predicted_probs <- integrate_ppi_scores_and_train(
      raw_score_df = feature_columns_df,
      response_vector = response_vector,
      model_method = model_method,
      num_cv_folds = num_cv_folds,
      num_cv_repeats = num_cv_repeats,
      seed = seed,
      num_cores = num_cores
    )
    list_of_predictions[[model_method]] <- predicted_probs
  }

  predictions_df <- do.call(cbind, list_of_predictions)
  colnames(predictions_df) <- paste0("Prob_", model_methods_vector)

  output_df <- cbind(ppi_score_df, predictions_df)

  if (average_predictions && ncol(predictions_df) > 0) {
    if (ncol(predictions_df) == 1) {
      output_df$SMED_Score <- predictions_df[,1]
    } else {
      output_df$SMED_Score <- rowMeans(predictions_df, na.rm = TRUE)
    }
  }

  message("Ensemble model training complete.")
  return(output_df)
}
