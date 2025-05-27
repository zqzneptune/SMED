#' Benchmark PPI Scores Against Reference Interactions
#'
#' Calculates a ROC curve object to benchmark a list of predicted
#' Protein-Protein Interactions (PPIs) and their scores against a reference
#' interactome (True Positives and True Negatives).
#'
#' @param ppi_vector A character vector of PPIs, where each element is a
#'   string identifying a unique interaction (e.g., "ProteinA~ProteinB").
#' @param score_vector A numeric vector of scores corresponding to each PPI in
#'   `ppi_vector`. Must be the same length as `ppi_vector`.
#' @param reference_interactome A list containing two data frames:
#'   `TP` (True Positives) and `TN` (True Negatives). Each data frame must
#'   contain a column named "PPI" with PPI identifiers in the same format
#'   as `ppi_vector`.
#'
#' @return A ROC object from the `pROC` package.
#' @export
#' @importFrom pROC roc
#' @importFrom dplyr filter
#' @importFrom magrittr `%>%`
#' @examples
#' # Create dummy data
#' ppi_preds <- paste0("P", 1:10, "~P", 11:20)
#' scores <- runif(10)
#' ref_tp <- data.frame(PPI = ppi_preds[1:5], stringsAsFactors = FALSE)
#' ref_tn <- data.frame(PPI = ppi_preds[6:8], stringsAsFactors = FALSE)
#' reference_set <- list(TP = ref_tp, TN = ref_tn)
#'
#' # Run benchmarking
#' if (requireNamespace("pROC", quietly = TRUE)) {
#'   roc_obj <- benchmark_ppi_score(ppi_preds, scores, reference_set)
#'   # print(roc_obj)
#'   # plot(roc_obj)
#' }
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

  # Extract unique proteins involved in the predicted PPIs
  # This step was in the original code to filter reference_interactome.
  # However, typically, the reference_interactome is fixed, and predictions
  # are evaluated against it. If ppi_vector contains PPIs with proteins
  # not in the universe considered by reference_interactome, they might
  # become NA in response. This filtering step is kept as per original logic,
  # assuming reference_interactome might be larger than necessary.
  proteins_in_query <-
    unique(unlist(strsplit(ppi_vector, "~")))

  # Filter reference interactions to those involving proteins in the query set
  # This step might be computationally intensive for large reference sets
  # and many unique proteins.
  # Original:
  # ref_filtered <-
  # lapply(reference_interactome, function(raw_df){
  #   dat <- raw_df %>%
  #     filter((`InteractorA` %in% proteins_in_query) &
  #            (`InteractorB` %in% proteins_in_query))
  #   return(dat)
  # })
  # The above assumes InteractorA and InteractorB columns. The problem states
  # reference_interactome$TP and $TN have a "PPI" column.
  # Let's assume PPIs in reference_interactome are already formed correctly.
  # The filtering logic would be to ensure *both* partners of a reference PPI
  # are among `proteins_in_query`.

  # Simpler approach: directly use the PPI strings for matching.
  # The original filtering based on individual proteins in `proteins_in_query`
  # is only relevant if `reference_interactome` lists `InteractorA` and
  # `InteractorB` and needs to be reconstructed.
  # Given the problem states `reference_interactome$TP$PPI`, we use that.

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
