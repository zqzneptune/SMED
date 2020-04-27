GetCOAPEX <- function(e){
  e <-
    e[sort(rownames(e)), ] ### Pre-sort to generate PPI
  f <- e
  f[f == 0] <- NA
  rawGauss <-
    PrInCE::build_gaussians(f)
  rawCOAPEX <-
    PrInCE::co_apex(rawGauss)
  coapex <-
    rawCOAPEX[lower.tri(rawCOAPEX, diag = FALSE)]
  s <-
    RcppAlgos::comboGeneral(rownames(rawCOAPEX), 2)
  datPPI <-
    data.frame(`PPI` = paste(s[, 1], s[, 2], sep = "~"),
               `COAPEX` = coapex,
               stringsAsFactors = FALSE)
  return(datPPI)
}
