GetPC <- function(e){
  e <-
    e[sort(rownames(e)), ] ### Pre-sort to generate PPI
  message("Compute Dice ...")
  dice <-
    as.matrix(arules::dissimilarity((!is.na(e) & e > 0), method = 'dice'))
  message("Compute Rho_p ...")
  rho_p <-
    propr::perb(t(e))@matrix
  message("Compute Phi_s ...")
  phi_s =
    propr::phis(t(e))@matrix

  message("Compute WCC ...")
  wccs <-
    apply(RcppAlgos::comboGeneral(seq_len(nrow(e)), 2), 1, function(x){
      wccsom::wcc(e[x[1], ], e[x[2], ], trwdth = 1)
    })
  s <-
    RcppAlgos::comboGeneral(rownames(e), 2)
  datPPI <-
    data.frame(`PPI` = paste(s[, 1], s[, 2], sep = "~"),
               `DICE` = dice[lower.tri(dice, diag = FALSE)],
               `RHO` = rho_p[lower.tri(rho_p, diag = FALSE)],
               `PHI` = phi_s[lower.tri(phi_s, diag = FALSE)],
               `WCC` = wccs,
               stringsAsFactors = FALSE)
  return(datPPI)
}
