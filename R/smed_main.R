#' Aggregate Scores from Multiple Elution-Based Scoring Methods
#'
#' This function serves as an aggregator for various PPI scoring methods based
#' on protein elution profiles. It calls individual scoring functions
#' (e.g., CoApex, DICE, MI, PCCN, WCC) and merges their outputs into a
#' single data frame where rows are unique PPIs and columns are the different
#' scores.
#'
#' @param elution_matrix A numeric matrix where rows are proteins (named) and
#'   columns are fractions.
#' @param min_fractions_default Integer, a default value for
#'   `min_fractions_present` used across scoring functions that accept it.
#'   Individual functions might have their own specific defaults if not
#'   overridden by parameters passed via `...`. Default 2.
#' @param coapex_args List, arguments to pass to `score_ppi_by_coapex`.
#' @param dice_args List, arguments to pass to `score_ppi_by_dice`.
#' @param mi_args List, arguments to pass to `score_ppi_by_mi`.
#' @param pccn_args List, arguments to pass to `score_ppi_by_pccn`.
#' @param wcc_args List, arguments to pass to `score_ppi_by_wcc`.
#' @param verbose Logical, if `TRUE`, enables verbose output from aggregator and
#'   potentially underlying functions (if they support it). Default `TRUE`.
#'
#' @return A data frame where the first column is "PPI" and subsequent columns
#'   are scores from the different methods (e.g., "norm_coapex_score",
#'   "dice_similarity_score", "mi_score", "pccn_score", "wcc_score").
#'   PPIs that are not scored by a particular method will have `NA` in that
#'   method's column.
#' @export
#' @importFrom dplyr full_join
#' @seealso \code{\link{score_ppi_by_coapex}}, \code{\link{score_ppi_by_dice}},
#'   \code{\link{score_ppi_by_mi}}, \code{\link{score_ppi_by_pccn}},
#'   \code{\link{score_ppi_by_wcc}}
#' @examples
#' # Create a dummy elution matrix
#' set.seed(123)
#' mat <- matrix(rpois(10 * 15, lambda=3), nrow = 10, ncol = 15)
#' rownames(mat) <- paste0("Prot", 1:10)
#' mat[1, 3:5] <- mat[1, 3:5] + 10 # Peak for Prot1
#' mat[2, 4:6] <- mat[2, 4:6] + 10 # Peak for Prot2 (overlap)
#'
#' # Reduce parameters for a quick example run
#' if (requireNamespace("infotheo", quietly = TRUE) &&
#'     requireNamespace("proxy", quietly = TRUE) &&
#'     requireNamespace("ptw", quietly = TRUE) &&
#'     requireNamespace("arules", quietly = TRUE)) {
#'
#'   aggregated_scores <- aggregate_elution_scores(
#'     elution_matrix = mat,
#'     min_fractions_default = 1,
#'     coapex_args = list(top_n_ppi = 50),
#'     dice_args = list(top_n_ppi = 50),
#'     mi_args = list(top_n_ppi = 50, num_bins = 2),
#'     pccn_args = list(top_n_ppi = 50, num_replicates = 3, verbose = FALSE),
#'     wcc_args = list(top_n_ppi = 50, window_width = 1),
#'     verbose = FALSE
#'   )
#'   # print(head(aggregated_scores))
#'   # print(dim(aggregated_scores))
#' }
aggregate_elution_scores <- function(
    elution_matrix,
    min_fractions_default = 2,
    coapex_args = list(),
    dice_args = list(),
    mi_args = list(),
    pccn_args = list(),
    wcc_args = list(),
    verbose = TRUE) {

  if (verbose) message("Aggregating elution scores...")

  all_score_dfs <- list()

  # Helper to call scoring function with args
  call_scorer <- function(scorer_fun, args_list, fun_name) {
    # Set min_fractions_present if not already in args_list
    if (!"min_fractions_present" %in% names(args_list)) {
      args_list$min_fractions_present <- min_fractions_default
    }
    # Add elution_matrix as first argument
    call_args <- c(list(elution_matrix = elution_matrix), args_list)
    if (verbose) message("  Scoring with: ", fun_name)
    tryCatch(
      do.call(scorer_fun, call_args),
      error = function(e) {
        warning("Error in ", fun_name, ": ", e$message, call.=FALSE)
        # Return empty df with PPI column to allow merge to proceed
        return(data.frame(PPI=character(0)))
      }
    )

  }

  # ScoreCoApex
  all_score_dfs$coapex <- call_scorer(score_ppi_by_coapex, coapex_args,
                                      "score_ppi_by_coapex")

  # ScoreDICE
  all_score_dfs$dice <- call_scorer(score_ppi_by_dice, dice_args,
                                    "score_ppi_by_dice")
  # ScoreMI
  if (requireNamespace("infotheo", quietly = TRUE)) {
    all_score_dfs$mi <- call_scorer(score_ppi_by_mi, mi_args, "score_ppi_by_mi")
  } else {
    if (verbose) message("  Skipping MI scoring: 'infotheo' package not available.")
  }

  # ScorePCCN
  all_score_dfs$pccn <- call_scorer(score_ppi_by_pccn, pccn_args,
                                    "score_ppi_by_pccn")

  # ScoreWCC
  if (requireNamespace("proxy", quietly = TRUE) &&
      requireNamespace("ptw", quietly = TRUE)) {
    all_score_dfs$wcc <- call_scorer(score_ppi_by_wcc, wcc_args, "score_ppi_by_wcc")
  } else {
    if (verbose) message("  Skipping WCC scoring: 'proxy' or 'ptw' not available.")
  }

  # Filter out NULLs or data frames without PPI column (from errors)
  valid_score_dfs <- Filter(
    function(df) !is.null(df) && "PPI" %in% colnames(df) && nrow(df) > 0,
    all_score_dfs
  )

  if (length(valid_score_dfs) == 0) {
    if(verbose) message("No valid scores generated by any method.")
    return(data.frame(PPI=character(0)))
  }


  # Merge all score data frames using dplyr::full_join for robustness
  # Start with an empty PPI data frame if the first valid score df is problematic
  # or create one based on all unique PPIs encountered.
  # A simpler Reduce should work if all df are well-behaved (have "PPI" column)
  merged_scores_df <- Reduce(
    function(df1, df2) dplyr::full_join(df1, df2, by = "PPI"),
    valid_score_dfs
  )

  if (verbose) message("Finished aggregating scores. Total unique PPIs scored: ",
                       nrow(merged_scores_df))
  return(merged_scores_df)
}


#' Perform Score Matrix Ensemble Discovery (SMED) for PPIs
#'
#' This is a high-level workflow function that first aggregates various
#' elution-based scores for PPIs from a raw elution matrix, and then uses these
#' scores as features to train an ensemble of machine learning models. The goal
#' is to predict PPIs based on a combined evidence approach.
#'
#' @param raw_elution_matrix A numeric matrix where rows are proteins (named) and
#'   columns are fractions, representing elution profiles.
#' @param reference_train_interactome A list for training ML models, containing
#'   `TP` (True Positive) and `TN` (True Negative) PPI data frames. Each must
#'   have a "PPI" column.
#' @param model_methods A character vector of `caret` model method names for
#'   the ensemble (e.g., c("rf", "glmnet")).
#' @param aggregate_scorer_args List of arguments to pass to
#'   `aggregate_elution_scores`. This can include specific argument lists for
#'   each sub-scorer (e.g., `coapex_args`, `mi_args`).
#' @param ensemble_trainer_args List of arguments to pass to
#'   `train_ensemble_ppi_models` (e.g., `num_cv_folds`, `num_cv_repeats`).
#' @param verbose Logical, if `TRUE`, enables verbose output. Default `TRUE`.
#'
#' @return A data frame, typically the output of
#'   `train_ensemble_ppi_models`, containing the initial PPIs, their aggregated
#'   feature scores, individual model prediction probabilities, and an overall
#'   ensemble score (e.g., "SMED_Score").
#' @export
#' @seealso \code{\link{aggregate_elution_scores}},
#'   \code{\link{train_ensemble_ppi_models}}
#' @examples
#' \dontrun{
#' # This example uses data loader functions from SMED
#' # Ensure you replace "SMED" with the actual name of your package.
#'
#' if (requireNamespace("SMED", quietly = TRUE) && # Check your own package
#'     requireNamespace("caret", quietly = TRUE) &&
#'     requireNamespace("doParallel", quietly = TRUE) &&
#'     requireNamespace("Hmisc", quietly = TRUE) &&
#'     requireNamespace("randomForest", quietly = TRUE) &&
#'     requireNamespace("RcppAlgos", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'
#'   # Load the data using wrapper functions from your package
#'   # (Assuming these functions are exported from SMED)
#'   raw_elution_matrix <- SMED::load_cofrac_293NE12_hcw()
#'   reference_complex_list <- SMED::load_reference_corum_havugimana()
#'
#'   # For the example, let's subset the elution matrix
#'   set.seed(123)
#'   if (nrow(raw_elution_matrix) > 50) {
#'     protein_subset_indices <- sample(rownames(raw_elution_matrix), 50)
#'     raw_elution_matrix_subset <- raw_elution_matrix[protein_subset_indices, ]
#'   } else {
#'     raw_elution_matrix_subset <- raw_elution_matrix
#'   }
#'
#'   all_elution_proteins <- rownames(raw_elution_matrix_subset)
#'   filtered_corum <- lapply(reference_complex_list, function(cpx) {
#'     intersect(cpx, all_elution_proteins)
#'   })
#'   filtered_corum <- Filter(function(cpx) length(cpx) >= 2, filtered_corum)
#'   if (length(filtered_corum) > 5) {
#'       filtered_corum <- filtered_corum[1:5]
#'   }
#'
#'   # Assuming generate_reference_ppi_sets is also in SMED
#'   reference_train_interactome <-
#'       SMED::generate_reference_ppi_sets(filtered_corum)
#'
#'   smed_results <- SMED::run_smed_workflow(
#'     raw_elution_matrix = raw_elution_matrix_subset,
#'     reference_train_interactome = reference_train_interactome,
#'     model_methods = c("rf"),
#'     aggregate_scorer_args = list(
#'       min_fractions_default = 1,
#'       coapex_args = list(top_n_ppi = NULL),
#'       dice_args = list(top_n_ppi = NULL),
#'       pccn_args = list(num_replicates = 2, verbose=FALSE),
#'       verbose = FALSE
#'     ),
#'     ensemble_trainer_args = list(
#'       num_cv_folds = 2, num_cv_repeats = 1, num_cores = 1
#'     ),
#'     verbose = TRUE
#'   )
#'
#'   print(head(smed_results))
#' } else {
#'   message(
#'     "Example for run_smed_workflow skipped: ",
#'     "required packages (including SMED itself) not available."
#'   )
#' }
#' } # End of \dontrun
#'
run_smed_workflow <- function(
    raw_elution_matrix,
    reference_train_interactome,
    model_methods,
    aggregate_scorer_args = list(),
    ensemble_trainer_args = list(),
    verbose = TRUE) {

  if (verbose) message("Starting SMED workflow...")

  # Step 1: Aggregate elution scores
  if (verbose) message("Step 1: Aggregating elution-based scores...")
  # Ensure elution_matrix is passed to aggregate_elution_scores
  agg_args_full <- c(list(elution_matrix = raw_elution_matrix,
                          verbose = verbose),
                     aggregate_scorer_args)
  aggregated_ppi_scores_df <- do.call(aggregate_elution_scores, agg_args_full)

  if (nrow(aggregated_ppi_scores_df) == 0 ||
      ncol(aggregated_ppi_scores_df) <= 1) { # Should have PPI + at least one score
    stop("Score aggregation resulted in no usable features. SMED workflow cannot proceed.")
  }


  # Step 2: Train ensemble models
  if (verbose) message("Step 2: Training ensemble of machine learning models...")
  trainer_args_full <- c(
    list(
      ppi_score_df = aggregated_ppi_scores_df,
      reference_train_interactome = reference_train_interactome,
      model_methods_vector = model_methods
    ),
    ensemble_trainer_args # User can override defaults like folds, repeats
  )
  # Ensure verbose is not passed if train_ensemble_ppi_models doesn't take it
  # train_ensemble_ppi_models has its own internal messaging.

  final_smed_scores_df <- do.call(train_ensemble_ppi_models,
                                  trainer_args_full)

  if (verbose) message("SMED workflow completed.")
  return(final_smed_scores_df)
}
