#' Impute and Preprocess Score Matrix for Machine Learning
#'
#' This internal helper function takes a raw score matrix, imputes missing
#' values, and applies preprocessing (e.g., centering, scaling) using
#' `caret::preProcess`.
#'
#' @param score_matrix A numeric matrix or data frame where rows are
#'   observations (e.g., PPIs) and columns are features (scores).
#'   The first column, if named "PPI", will be excluded from imputation
#'   and preprocessing but preserved.
#' @return A list containing:
#'   \item{preprocessed_data}{The preprocessed data frame.}
#'   \item{preprocessing_model}{The `preProcess` object from caret, which
#'     can be used to apply the same transformation to new data.}
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

  # Ensure feature_data is numeric
  feature_data_numeric <- as.data.frame(
    lapply(feature_data, function(col) {
      if(!is.numeric(col)) as.numeric(as.character(col)) else col
    })
  )
  rownames(feature_data_numeric) <- rownames(feature_data)


  # Impute missing values for each feature column
  # Hmisc::impute uses the median for numeric data by default.
  imputed_feature_data <- apply(feature_data_numeric, 2, function(x) {
    # Check if any non-NA values exist before imputing
    if (any(!is.na(x))) {
      return(Hmisc::impute(x, fun = stats::median)) # Or mean, etc.
    } else {
      return(x) # Column is all NA, return as is (or fill with 0)
    }
  })
  # apply returns a matrix, convert back to data frame if needed
  imputed_feature_data_df <- as.data.frame(imputed_feature_data)
  rownames(imputed_feature_data_df) <- rownames(feature_data_numeric)


  # Preprocess (e.g., center, scale) - default is center and scale
  # Ensure there's variance in columns for scaling
  # preProcess might throw error if a column has zero variance
  # Check for columns with zero variance after imputation
  col_sds <- apply(imputed_feature_data_df, 2, stats::sd, na.rm = TRUE)
  cols_to_process <- colnames(imputed_feature_data_df)[col_sds > 1e-6]

  preproc_model <- NULL
  preprocessed_data <- imputed_feature_data_df

  if (length(cols_to_process) > 0) {
    preproc_model <- caret::preProcess(
      imputed_feature_data_df[, cols_to_process, drop = FALSE],
      method = c("center", "scale") # Common defaults
    )
    preprocessed_subset <- predict(
      preproc_model,
      newdata = imputed_feature_data_df[, cols_to_process, drop = FALSE]
    )
    preprocessed_data[, cols_to_process] <- preprocessed_subset
  } else {
    message("No columns with sufficient variance found for preprocessing.")
  }


  # Re-attach PPI column if it existed
  if (!is.null(ppi_column)) {
    preprocessed_data <- cbind(ppi_column, preprocessed_data)
  }

  return(list(
    preprocessed_data = preprocessed_data,
    preprocessing_model = preproc_model
  ))
}


#' Train a Single Machine Learning Model and Predict Probabilities
#'
#' Trains a specified machine learning model using `caret` on provided scores
#' and responses. It handles data imputation and preprocessing.
#' This function is suitable for training one model at a time.
#'
#' @param raw_score_df A data frame or matrix of feature scores. Rows are
#'   observations (e.g., PPIs), columns are features.
#' @param response_vector A factor vector ("Y", "N") or numeric (0, 1)
#'   representing the response variable. Must have the same number of
#'   observations as `raw_score_df`. NAs are allowed and will be removed
#'   before training.
#' @param model_method Character string specifying the `caret` model method
#'   (e.g., "rf", "glmnet", "svmRadial").
#' @param num_cv_folds Integer, number of folds for cross-validation. Default 5.
#' @param num_cv_repeats Integer, number of repeats for cross-validation.
#'   Default 1.
#' @param seed Integer, for reproducibility. Default 100.
#' @param num_cores Integer, number of cores for parallel model training.
#'   Default is 2. Uses `doParallel` with `makePSOCKcluster`.
#'
#' @return A numeric vector of predicted probabilities for the "positive"
#'   class (assumed to be the second level of the factor response, e.g., "Y").
#' @export
#' @importFrom caret trainControl train twoClassSummary
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom pROC roc auc
#' @importFrom stats predict


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

  # Preprocess scores (impute, scale, center)
  preprocessing_result <- preprocess_score_matrix(raw_score_df)
  processed_scores_df <- preprocessing_result$preprocessed_data

  # Prepare training data: combine processed scores with response
  # Ensure response is a factor for classification
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

  # Remove observations with NA response
  training_data_complete <-
    training_data[!is.na(training_data$Response), ]

  if (nrow(training_data_complete) == 0) {
    stop("No complete cases left after removing NAs in response variable.")
  }
  # Check for sufficient data in each class
  class_summary <- summary(training_data_complete$Response)
  if (any(class_summary < num_cv_folds)) {
    warning("One or more classes have fewer samples than 'num_cv_folds'.",
            " This can cause issues with cross-validation. Consider reducing folds.")
  }
  if (any(class_summary == 0)) {
    stop("One class has zero samples. Cannot train model.")
  }


  set.seed(seed)
  fit_control <- caret::trainControl(
    method = "repeatedcv",
    number = num_cv_folds,
    repeats = num_cv_repeats,
    savePredictions = "final",
    classProbs = TRUE, # Crucial for probability predictions
    summaryFunction = caret::twoClassSummary,
    allowParallel = TRUE # Allow caret to use parallel backend
  )

  # Setup parallel backend
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
    suppressMessages( # Suppress caret's routine messages
      trained_model <- caret::train(
        Response ~ .,
        data = training_data_complete,
        method = model_method,
        trControl = fit_control,
        metric = "ROC" # Optimize for AUC
      )
    )
  }, error = function(e) {
    message("Error during model training with '", model_method, "': ", e$message)
    # trained_model remains NULL
  }, finally = {
    if (num_cores > 1 && exists("cl")) {
      # Cluster automatically stopped by stopImplicitCluster via on.exit
      # or could be explicit: doParallel::stopCluster(cl)
    }
  })

  if (is.null(trained_model)) {
    warning("Model training failed for method: ", model_method,
            ". Returning NAs.")
    return(rep(NA_real_, nrow(raw_score_df)))
  }

  # Predict probabilities on the original (potentially larger) dataset
  # (rows where response was NA will also get predictions)
  # The 'processed_scores_df' contains all original rows, preprocessed
  predicted_probs_all <- stats::predict(
    trained_model,
    newdata = processed_scores_df, # Use the full preprocessed set
    type = "prob"
  )

  # Return probabilities for the positive class (e.g., "Y")
  # The positive class is usually the second level of the factor
  positive_class_label <- levels(training_data_complete$Response)[2]
  return(predicted_probs_all[, positive_class_label])
}


#' Train Multiple Machine Learning Models and Ensemble Predictions
#'
#' Trains a suite of specified machine learning models using `caret` on PPI
#' feature scores and a reference training set of interactions.
#' Predictions from these models are then (optionally) averaged to produce an
#' ensemble score.
#'
#' @param ppi_score_df A data frame where the first column is "PPI" (character
#'   string, e.g., "ProteinA~ProteinB") and subsequent columns are numeric
#'   feature scores for these PPIs.
#' @param reference_train_interactome A list containing two data frames:
#'   `TP` (True Positives) and `TN` (True Negatives). Each data frame must
#'   contain a column named "PPI" with PPI identifiers. These are used to
#'   label PPIs in `ppi_score_df` for training.
#' @param model_methods_vector A character vector of `caret` model method names
#'   (e.g., c("rf", "glmnet")).
#' @param average_predictions Logical, if `TRUE` (default), an additional
#'   column "SMED_Score" (Simple Mean Ensemble D-score) with row-wise mean
#'   of probabilities from all models is added.
#' @param num_cv_folds Integer, number of folds for cross-validation. Default 5.
#' @param num_cv_repeats Integer, number of repeats for cross-validation.
#'   Default 10. (Note: original had 10, `integrate_ppi_scores_and_train` default 1)
#'   Let's make it 1 here for consistency with the single trainer default.
#' @param seed Integer, for reproducibility. Default 100.
#' @param num_cores Integer, number of cores for parallel training of each model.
#'   Default is 2.
#'
#' @return The input `ppi_score_df` augmented with columns for predicted
#'   probabilities from each trained model. If `average_predictions` is `TRUE`,
#'   an additional "SMED_Score" column is also included.
#' @export


train_ensemble_ppi_models <- function(
    ppi_score_df,
    reference_train_interactome,
    model_methods_vector,
    average_predictions = TRUE,
    num_cv_folds = 5,
    num_cv_repeats = 1, # Changed from 10 to 1 for consistency
    seed = 100,
    num_cores = 2) {

  if (!"PPI" %in% colnames(ppi_score_df)) {
    stop("'ppi_score_df' must contain a 'PPI' column.")
  }
  if (!is.list(reference_train_interactome) ||
      !all(c("TP", "TN") %in% names(reference_train_interactome))) {
    stop("'reference_train_interactome' must be a list with 'TP' and 'TN'.")
  }

  # Prepare response vector based on reference interactome
  response_vector <- ifelse(
    ppi_score_df$PPI %in% reference_train_interactome$TP$PPI, "Y",
    ifelse(ppi_score_df$PPI %in% reference_train_interactome$TN$PPI, "N", NA)
  )
  response_vector <- factor(response_vector, levels = c("N", "Y"))

  # Isolate feature columns (exclude "PPI" column)
  feature_columns_df <-
    ppi_score_df[, !(colnames(ppi_score_df) == "PPI"), drop = FALSE]

  # Store predictions from each model
  list_of_predictions <- list()

  message("Starting ensemble model training for: ",
          paste(model_methods_vector, collapse=", "))

  for (model_method in model_methods_vector) {
    message("  Training model: ", model_method, "...")
    set.seed(seed) # Ensure reproducibility for each model's training

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

  # Combine predictions into a data frame
  predictions_df <- do.call(cbind, list_of_predictions)
  colnames(predictions_df) <- paste0("Prob_", model_methods_vector)

  # Augment original score data frame
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
