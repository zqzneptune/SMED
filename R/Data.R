#' Dummy Protein Elution Matrix
#'
#' A synthetic elution matrix for demonstrating the SMED pipeline.
#'
#' @format A numeric matrix with 10 rows (proteins) and 50 columns (fractions).
#' @source Synthetic data generated for package examples.
"dummy_elution_matrix"

#' Dummy Reference Complexes for Training
#'
#' A list containing True Positive (TP) and True Negative (TN) PPI pairs for training.
#'
#' @format A list with two elements:
#' \describe{
#'   \item{TP}{Data frame of true positive interactions.}
#'   \item{TN}{Data frame of true negative interactions.}
#' }
#' @source Synthetic data generated for package examples.
"dummy_train_complexes"
