EultionScore <- function(mRaw){
  d <-
    mRaw[rowSums(mRaw, na.rm = TRUE) != 0, ]
  e <-
    d[sort(rownames(d)), ]
  s <-
    RcppAlgos::comboGeneral(rownames(e), 2)
  message("Computing Dice ...")
  dice <-
    as.matrix(arules::dissimilarity((!is.na(e) & e > 0), method = "dice"))
  message("Computing Phi ...")
  phi <-
    propr::propr(t(e), metric = "phi", symmetrize = TRUE)@matrix
  message("Computing Rho_p ...")
  rho <-
    propr::propr(t(e), metric = "rho", ivar = 0)@matrix
  message("Computing Phi_s ...")
  phs <-
    propr::propr(t(e), metric = "phs", ivar = 0)@matrix

  message("Compute WCC ...")
  wccs <-
    SMED::WCCS(e)
  message("Compute PCCN ...")
  pccn <-
    SMED::PCCN(mRaw = e, rept = 10)
  datPPI <-
    data.frame(`PPI` = paste(s[, 1], s[, 2], sep = "~"),
               `DICE` = dice[lower.tri(dice, diag = FALSE)],
               `PHI` = phi[lower.tri(phi, diag = FALSE)],
               `RHO` = rho[lower.tri(rho, diag = FALSE)],
               `PHS` = phs[lower.tri(phs, diag = FALSE)],
               `WCC` = wccs,
               `PCCN` = pccn,
               stringsAsFactors = FALSE)
  return(datPPI)
}
