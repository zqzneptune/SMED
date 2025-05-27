# R/data_loaders.R

#' Load Example CoFrac Elution Matrix (Havugimana 2012, 293NE12 HCW)
#'
#' Loads an example co-fractionation count matrix from Havugimana et al., 2012,
#' specifically the LTQ 293NE12 HCW dataset.
#'
#' @return A numeric matrix representing protein elution profiles.
#' @export
#' @examples
#' if (interactive()) { # Avoid running automatically during checks if large
#'   cofrac_293NE12 <- load_cofrac_293NE12_hcw()
#'   # print(dim(cofrac_293NE12))
#'   # print(head(cofrac_293NE12[,1:5]))
#' }
load_cofrac_293NE12_hcw <- function() {
  data_file_path <- system.file(
    "extdata",
    "CoFrac_Havugimana_PC_Cell_2012_LTQ_293NE12_HCW.RDS",
    package = "SMED"
  )
  if (!nzchar(data_file_path)) { # nzchar checks for non-empty string
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
#' specifically the LTQ HeLaNE45 TCS dataset.
#'
#' @return A numeric matrix representing protein elution profiles.
#' @export
#' @examples
#' if (interactive()) {
#'   cofrac_HeLaNE45 <- load_cofrac_HeLaNE45_tcs()
#'   # print(dim(cofrac_HeLaNE45))
#' }
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
#' specifically the LTQ HeLaCE12 TCS dataset.
#'
#' @return A numeric matrix representing protein elution profiles.
#' @export
#' @examples
#' if (interactive()) {
#'   cofrac_HeLaCE12 <- load_cofrac_HeLaCE12_tcs()
#'   # print(dim(cofrac_HeLaCE12))
#' }
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
#' relevant to the Havugimana et al., 2012 study context.
#'
#' @return A list where each element is a character vector of protein
#'   identifiers representing a complex.
#' @export
#' @examples
#' if (interactive()) {
#'   corum_complexes <- load_reference_corum_havugimana()
#'   # print(length(corum_complexes))
#'   # if (length(corum_complexes) > 0) print(head(corum_complexes[[1]])))
#' }
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
#' Loads an example list of predicted protein complexes,
#' relevant to the Havugimana et al., 2012 study context.
#'
#' @return A list where each element is a character vector of protein
#'   identifiers representing a complex.
#' @export
#' @examples
#' if (interactive()) {
#'   pred_complexes <- load_pred_cpx_havugimana()
#'   # print(length(pred_complexes))
#'   # if (length(pred_complexes) > 0) print(head(pred_complexes[[1]])))
#' }
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
