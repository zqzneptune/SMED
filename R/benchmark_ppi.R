#' Benchmark PPI Scores Against Reference Interactions
#'
#' Evaluates predicted Protein-Protein Interactions (PPIs) by comparing against
#' a gold standard reference interactome using Receiver Operating Characteristic
#' (ROC) analysis. This function provides quantitative assessment of interaction
#' prediction quality.
#'
#' @section Benchmarking Methodology:
#' The function implements a standard ROC analysis workflow:
#' 1. Matches predicted PPIs against reference True Positives (TP) and 
#' True Negatives (TN)
#' 2. Assigns binary labels (1 for TP matches, 0 for TN matches)
#' 3. Computes ROC curve using the pROC package
#' 4. Returns ROC object for further analysis and visualization
#'
#' @section Evaluation Metrics:
#' The ROC curve provides several key metrics:
#' - AUC (Area Under Curve): Overall prediction accuracy (0.5 = random,
#'  1 = perfect)
#' - Sensitivity/Recall: True Positive Rate
#' - Specificity: True Negative Rate
#' - Optimal cutoff: Balance between sensitivity and specificity
#'
#' @section Performance Assessment:
#' The function automatically checks for:
#' - Sufficient overlap between predictions and reference
#' - Presence of both positive and negative classes
#' - Valid score distributions
#' Warnings are issued for potential analysis issues.
#'
#' @section Biological Validation:
#' For meaningful results:
#' - Reference interactome should represent biologically validated interactions
#' - True Negatives should be carefully curated non-interacting pairs
#' - Consider protein complex co-membership as additional validation
#'
#' @param ppi_vector A character vector of PPIs in "ProteinA~ProteinB" format.
#'   Must use same identifier system as reference. Non-canonical pairs will be
#'   treated as NA in response.
#' @param score_vector Numeric vector of confidence scores for each PPI. Higher
#'   scores should indicate higher confidence in interaction. Scores will be
#'   treated as continuous predictors in ROC analysis.
#' @param reference_interactome List containing two data frames:
#'   - TP: True Positives with "PPI" column
#'   - TN: True Negatives with "PPI" column
#'   Reference PPIs should use same format as ppi_vector.
#'
#' @section Parameter Effects:
#' - Score distribution affects ROC shape (uniform vs skewed)
#' - Reference set quality impacts validity (balanced TP/TN recommended)
#' - Missing values in scores will be excluded
#' - Non-matching PPIs become NA and are excluded
#'
#' @return An object of class "roc" from pROC package containing:
#' \itemize{
#'   \item auc - Area Under the Curve value
#'   \item sensitivities - Sensitivity at various thresholds
#'   \item specificities - Specificity at various thresholds
#'   \item thresholds - Score thresholds used
#'   \item direction - Direction of ROC calculation
#' }
#' Returns NULL if insufficient data for analysis.
#'
#' @export
#' @importFrom pROC roc
#' @importFrom dplyr filter
#' @importFrom magrittr `%>%`
#'
#' @examples
#' # Load example reference data from package
#' data("RefCORUM_Havugimana_PC_Cell_2012_n_324")
#' data("PredCpx_Havugimana_PC_Cell_2012_n_622")
#'
#' # Generate all possible PPIs from predicted complexes
#' pred_ppis <- generate_all_pairwise_ppi(PredCpx_Havugimana_PC_Cell_2012_n_622)
#'
#' # Create reference set (TP = within same CORUM complex, 
#' TN = between complexes)
#' ref_set <- 
#' generate_reference_ppi_sets(RefCORUM_Havugimana_PC_Cell_2012_n_324)
#'
#' # Score PPIs using co-apex method (example scoring function)
#' scores <- score_ppi_by_coapex(pred_ppis, 
#'     inst/extdata/CoFrac_Havugimana_PC_Cell_2012_LTQ_293NE12_HCW.RDS)
#'
#' # Benchmark performance
#' roc_result <- benchmark_ppi_score(names(scores), scores, ref_set)
#'
#' # Analyze results
#' if (!is.null(roc_result)) {
#'   print(pROC::auc(roc_result))  # Overall AUC
#'   plot(roc_result)              # Visualize performance
#'   coords <- pROC::coords(roc_result, "best")  # Optimal cutoff
#' }
#'
#' @seealso \code{\link{benchmark_ppi_score}} man page for additional details.
#'   Related functions: \code{\link{generate_reference_ppi_sets}},
#'   \code{\link{generate_all_pairwise_ppi}}, \code{\link{score_ppi_by_coapex}}
benchmark_ppi_score <- function(ppi_vector,
                              score_vector,
                              reference_interactome) {
  if (length(ppi_vector) != length(score_vector)) {
    stop("PPI vector and score vector lengths must match.")
  }
  if (!is.list(reference_interactome) ||
      !all(c("TP", "TN") %in% names(reference_interactome))) {
    stop("reference_interactome must be a list with 'TP' and 'TN' elements.")
  }
  if (!is.data.frame(reference_interactome$TP) ||
      !"PPI" %in% colnames(reference_interactome$TP)) {
    stop("reference_interactome$TP must be a data frame with a 'PPI' column.")
  }
  if (!is.data.frame(reference_interactome$TN) ||
      !"PPI" %in% colnames(reference_interactome$TN)) {
    stop("reference_interactome$TN must be a data frame with a 'PPI' column.")
  }

  proteins_in_query <-
    unique(unlist(strsplit(ppi_vector, "~")))

  response_vector <- ifelse(
    ppi_vector %in% reference_interactome$TP$PPI,
    1,
    ifelse(ppi_vector %in% reference_interactome$TN$PPI, 0, NA)
  )

  if (all(is.na(response_vector))) {
    warning("No overlap found between ppi_vector and reference_interactome.",
            " ROC analysis not possible.")
    return(NULL)
  }
  if (length(unique(response_vector[!is.na(response_vector)])) < 2) {
    warning("ROC analysis requires at least two classes in the response.",
            " Check your reference interactome and PPI vector.")
    return(NULL)
  }

  roc_object <- pROC::roc(
    predictor = score_vector,
    response = response_vector,
    na.rm = TRUE,
    quiet = TRUE
  )

  return(roc_object)
}
