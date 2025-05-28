#' Load Example CoFrac Elution Matrix (Havugimana 2012, 293NE12 HCW)
#'
#' Loads an example co-fractionation count matrix from Havugimana et al., 2012,
#' specifically the LTQ 293NE12 HCW dataset. This data represents protein
#' co-fractionation profiles from human embryonic kidney cells (HEK293) under
#' non-enriched (NE) conditions with high confidence windows (HCW) filtering.
#'
#' @details
#' Data originally published in Havugimana et al. Cell 2012 (PMID: 22939625).
#' 
#' Supported file format: RDS (R binary serialization format)
#' 
#' Data structure requirements:
#' - Must be a numeric matrix with proteins as rows and fractions as columns
#' - Row names should be UniProt protein identifiers
#' - Column names should be fraction numbers (1-96)
#' 
#' @section Performance Considerations:
#' The matrix is approximately 15MB in memory (dimensions: 293 proteins x 96 fractions).
#' For large-scale analyses, consider:
#' - Subsetting to proteins of interest first
#' - Using sparse matrix representations if data is sparse
#' - Processing in chunks for memory-intensive operations
#'
#' @return A numeric matrix representing protein elution profiles where:
#'   - Rows are proteins (row names are UniProt IDs)
#'   - Columns are fractions (1-96)
#'   - Values are MS1 intensity measurements
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("SMED", quietly = TRUE)) {
#'   # Load the data
#'   cofrac_293NE12 <- load_cofrac_293NE12_hcw()
#'   
#'   # Basic inspection
#'   dim(cofrac_293NE12)
#'   colnames(cofrac_293NE12)[1:5]  # First 5 fractions
#'   rownames(cofrac_293NE12)[1:5]  # First 5 proteins
#'   range(cofrac_293NE12)          # Value range
#'   
#'   # See vignette("01-scoring") for analysis examples
#' }
#' }
#' @seealso \code{\link{load_cofrac_HeLaNE45_tcs}}, 
#'   \code{\link{load_cofrac_HeLaCE12_tcs}},
#'   \code{\link{prepare_elution_matrix}} for downstream processing
load_cofrac_293NE12_hcw <- function() {
  data_file_path <- system.file(
    "extdata",
    "CoFrac_Havugimana_PC_Cell_2012_LTQ_293NE12_HCW.RDS",
    package = "SMED"
  )
  if (!nzchar(data_file_path)) {
    stop(
      "Data file 'CoFrac_Havugimana_PC_Cell_2012_LTQ_293NE12_HCW.RDS'",
      " not found in package 'SMED'. Ensure package is installed correctly."
    )
  }
  return(readRDS(data_file_path))
}

#' Load Example CoFrac Elution Matrix (Havugimana 2012, HeLaNE45 TCS)
#'
#' Loads an example co-fractionation count matrix from Havugimana et al., 2012,
#' specifically the LTQ HeLaNE45 TCS dataset. This data represents protein
#' co-fractionation profiles from HeLa cells under non-enriched (NE) conditions
#' with total cell lysate (TCS) preparation.
#'
#' @details
#' Data originally published in Havugimana et al. Cell 2012 (PMID: 22939625).
#' 
#' Supported file format: RDS (R binary serialization format)
#' 
#' Data structure requirements:
#' - Must be a numeric matrix with proteins as rows and fractions as columns
#' - Row names should be UniProt protein identifiers
#' - Column names should be fraction numbers (1-96)
#' 
#' @section Performance Considerations:
#' The matrix is approximately 18MB in memory (dimensions: 450 proteins x 96 fractions).
#' Contains more proteins than the 293NE12 dataset. Consider:
#' - Using sparse matrix operations if working with zero-inflated data
#' - Pre-filtering proteins by abundance if memory constrained
#'
#' @return A numeric matrix representing protein elution profiles where:
#'   - Rows are proteins (row names are UniProt IDs)
#'   - Columns are fractions (1-96)
#'   - Values are MS1 intensity measurements
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("SMED", quietly = TRUE)) {
#'   # Load the data
#'   cofrac_HeLaNE45 <- load_cofrac_HeLaNE45_tcs()
#'   
#'   # Basic inspection
#'   str(cofrac_HeLaNE45)  # Structure overview
#'   summary(as.vector(cofrac_HeLaNE45))  # Value distribution
#'   
#'   # See vignette("01-scoring") for analysis examples
#' }
#' }
#' @seealso \code{\link{load_cofrac_293NE12_hcw}}, 
#'   \code{\link{load_cofrac_HeLaCE12_tcs}},
#'   \code{\link{filter_proteins_by_elution_consistency}} for quality control
load_cofrac_HeLaNE45_tcs <- function() {
  data_file_path <- system.file(
    "extdata",
    "CoFrac_Havugimana_PC_Cell_2012_LTQ_HeLaNE45_TCS.RDS",
    package = "SMED"
  )
  if (!nzchar(data_file_path)) {
    stop(
      "Data file 'CoFrac_Havugimana_PC_Cell_2012_LTQ_HeLaNE45_TCS.RDS'",
      " not found in package 'SMED'."
    )
  }
  return(readRDS(data_file_path))
}

#' Load Example CoFrac Elution Matrix (Havugimana 2012, HeLaCE12 TCS)
#'
#' Loads an example co-fractionation count matrix from Havugimana et al., 2012,
#' specifically the LTQ HeLaCE12 TCS dataset. This data represents protein
#' co-fractionation profiles from HeLa cells under crude extract (CE) conditions
#' with total cell lysate (TCS) preparation.
#'
#' @details
#' Data originally published in Havugimana et al. Cell 2012 (PMID: 22939625).
#' 
#' Supported file format: RDS (R binary serialization format)
#' 
#' Data structure requirements:
#' - Must be a numeric matrix with proteins as rows and fractions as columns
#' - Row names should be UniProt protein identifiers
#' - Column names should be fraction numbers (1-96)
#' 
#' @section Performance Considerations:
#' The matrix is approximately 20MB in memory (dimensions: 512 proteins x 96 fractions).
#' This is the largest of the three co-fractionation datasets. Consider:
#' - Using memory-efficient data structures for large analyses
#' - Processing in batches if analyzing multiple datasets
#'
#' @return A numeric matrix representing protein elution profiles where:
#'   - Rows are proteins (row names are UniProt IDs)
#'   - Columns are fractions (1-96)
#'   - Values are MS1 intensity measurements
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("SMED", quietly = TRUE)) {
#'   # Load the data
#'   cofrac_HeLaCE12 <- load_cofrac_HeLaCE12_tcs()
#'   
#'   # Basic inspection
#'   head(cofrac_HeLaCE12, n = c(5, 5))  # First 5 rows and columns
#'   sum(cofrac_HeLaCE12 == 0) / length(cofrac_HeLaCE12)  # Sparsity
#'   
#'   # See vignette("01-scoring") for analysis examples
#' }
#' }
#' @seealso \code{\link{load_cofrac_293NE12_hcw}}, 
#'   \code{\link{load_cofrac_HeLaNE45_tcs}},
#'   \code{\link{filter_matrix_by_nonzero_fractions}} for data filtering
load_cofrac_HeLaCE12_tcs <- function() {
  data_file_path <- system.file(
    "extdata",
    "CoFrac_Havugimana_PC_Cell_2012_LTQ_HeLaCE12_TCS.RDS",
    package = "SMED"
  )
  if (!nzchar(data_file_path)) {
    stop(
      "Data file 'CoFrac_Havugimana_PC_Cell_2012_LTQ_HeLaCE12_TCS.RDS'",
      " not found in package 'SMED'."
    )
  }
  return(readRDS(data_file_path))
}

#' Load Example CORUM Reference Complexes (Havugimana 2012 subset)
#'
#' Loads an example list of CORUM reference protein complexes,
#' relevant to the Havugimana et al., 2012 study context. This contains
#' 324 manually curated mammalian protein complexes from the CORUM database.
#'
#' @details
#' Data originally published in Havugimana et al. Cell 2012 (PMID: 22939625)
#' and derived from the CORUM database (PMID: 29145629).
#' 
#' Supported file format: RDS (R binary serialization format)
#' 
#' Data structure requirements:
#' - Must be a named list where each element represents a complex
#' - Each complex should be a character vector of UniProt protein identifiers
#' - Complex names should follow CORUM naming conventions
#' 
#' @section Performance Considerations:
#' The object is approximately 5MB in memory (324 complexes). Operations that
#' require pairwise comparison of complexes may scale quadratically with the
#' number of complexes. Consider:
#' - Pre-filtering to complexes of interest
#' - Using efficient data structures for set operations
#'
#' @return A list where:
#'   - Each element is a character vector of UniProt protein identifiers
#'   - List is named with CORUM complex identifiers (e.g., "CORUM_1234")
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("SMED", quietly = TRUE)) {
#'   # Load the data
#'   corum_complexes <- load_reference_corum_havugimana()
#'   
#'   # Basic inspection
#'   length(corum_complexes)  # Number of complexes
#'   names(corum_complexes)[1:5]  # First 5 complex names
#'   lengths(corum_complexes)[1:5]  # Sizes of first 5 complexes
#'   
#'   # See vignette("01-scoring") for benchmarking examples
#' }
#' }
#' @seealso \code{\link{load_pred_cpx_havugimana}},
#'   \code{\link{evaluate_complexes}} for performance evaluation
load_reference_corum_havugimana <- function() {
  data_file_path <- system.file(
    "extdata",
    "RefCORUM_Havugimana_PC_Cell_2012_n_324.RDS",
    package = "SMED"
  )
  if (!nzchar(data_file_path)) {
    stop(
      "Data file 'RefCORUM_Havugimana_PC_Cell_2012_n_324.RDS'",
      " not found in package 'SMED'."
    )
  }
  return(readRDS(data_file_path))
}

#' Load Example Predicted Complexes (Havugimana 2012 subset)
#'
#' Loads an example list of predicted protein complexes (622 complexes)
#' from Havugimana et al., 2012. These represent computationally predicted
#' complexes derived from co-fractionation MS data combined with other evidence.
#'
#' @details
#' Data originally published in Havugimana et al. Cell 2012 (PMID: 22939625).
#' 
#' Supported file format: RDS (R binary serialization format)
#' 
#' Data structure requirements:
#' - Must be a named list where each element represents a complex
#' - Each complex should be a character vector of UniProt protein identifiers
#' - Complex names indicate prediction confidence and CORUM overlaps
#' 
#' @section Performance Considerations:
#' The object is approximately 8MB in memory (622 complexes). Contains nearly
#' twice as many complexes as the reference set. Memory usage scales with:
#' - Number of complexes being analyzed
#' - Average complex size
#' - Number of pairwise comparisons needed
#'
#' @return A list where:
#'   - Each element is a character vector of UniProt protein identifiers
#'   - List is named with complex identifiers (format: "Pred_XXXX")
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("SMED", quietly = TRUE)) {
#'   # Load the data
#'   pred_complexes <- load_pred_cpx_havugimana()
#'   
#'   # Basic inspection
#'   length(pred_complexes)  # Number of complexes
#'   summary(lengths(pred_complexes))  # Size distribution
#'   table(grepl("CORUM", names(pred_complexes)))  # CORUM overlaps
#'   
#'   # See vignette("01-scoring") for benchmarking examples
#' }
#' }
#' @seealso \code{\link{load_reference_corum_havugimana}},
#'   \code{\link{benchmark_ppi_score}} for performance evaluation
load_pred_cpx_havugimana <- function() {
  data_file_path <- system.file(
    "extdata",
    "PredCpx_Havugimana_PC_Cell_2012_n_622.RDS",
    package = "SMED"
  )
  if (!nzchar(data_file_path)) {
    stop(
      "Data file 'PredCpx_Havugimana_PC_Cell_2012_n_622.RDS'",
      " not found in package 'SMED'."
    )
  }
  return(readRDS(data_file_path))
}
