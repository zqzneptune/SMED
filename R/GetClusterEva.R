GetClusterEva <- function(rawGS, rawPredict){
  library(parallel)
  num_cores <- detectCores() - 1
  # rawGS <- list(`A` = c("GG", "GAS", "KKS"),
  #               `B` = c("JACK", "LEKL", "LLK"),
  #               `C` = c("A", "C", "N"))
  # rawPredict <- list(`X` = c("GG", "GAS", "LL"),
  #                    `Y` = c("K", "C", "A"))
  dprt <-
    intersect(
      unlist(rawGS),
      unlist(rawPredict)
    )
  rawSizeGS <-
    unlist(lapply(rawGS, length))

  mpSizeGS <-
    unlist(lapply(rawGS, function(x){
      return(length(intersect(x, dprt)))
    }))
  cpxGS <-
    rawGS[mpSizeGS/rawSizeGS > 0.8]

  rawSizePred <-
    unlist(lapply(rawPredict, length))
  mpSizePred <-
    unlist(lapply(rawPredict, function(x){
      return(length(intersect(x, dprt)))
    }))
  cpxPredict <-
    rawPredict[mpSizePred/rawSizePred > 0.8]

  ### Start Computing ###
  sizeGS <-
    unlist(lapply(cpxGS, length))
  sizeGS <-
    unname(sizeGS)
  sizePredict <-
    unlist(lapply(cpxPredict, length))
  sizePredict <-
    unname(sizePredict)
  sizePvsGS <-
    unlist(lapply(cpxPredict, function(P){
      length(intersect(P, unique(unlist(cpxGS))))
    }))
  sizePvsGS <-
    unname(sizePvsGS)

  lsT <-
    mclapply(cpxGS, function(G) {
      sapply(cpxPredict, function(P) {
        length(intersect(G, P))
      })
    }, mc.cores = num_cores)

  prT <-
    matrix(
      unlist(lsT),
      byrow = TRUE,
      ncol = length(cpxPredict)
    )

  mPrT <-
    apply(prT, 1, max)
  Sn <-
    sum(mPrT)/sum(sizeGS) # from GS view

  nPrT <-
    apply(prT, 2, max)
  # PPV: positive predictive value

  PPV <-
    sum(nPrT)/sum(sizePvsGS) # from Pred view
  Acc <-
    sqrt(Sn * PPV)
  # Generally, a high Sn value indicates that the prediction has a good coverage
  # of the proteins in the true complexes, whereas a high PPV value indicates
  # that the predicted complexes are likely to be true complexes.
  lstJac <-
    mclapply(cpxGS, function(G) {
      sapply(cpxPredict, function(P) {
        length(intersect(G, P)) / length(union(G, P))
      })
    }, mc.cores = num_cores)

  prJac <-
    matrix(
      unlist(lstJac),
      byrow = TRUE,
      ncol = length(cpxPredict)
    )
  mJacG <-
    apply(prJac, 1, max)
  jacG <-
    sum(mJacG * sizeGS) / sum(sizeGS)
  mJacP <-
    apply(prJac, 2, max)
  jacP <-
    sum(mJacP * sizePredict) / sum(sizePredict)
  Jaccd <-
    2 * jacP * jacG /(jacP + jacG)

  lstPR <-
    mclapply(cpxGS, function(G) {
      sapply(cpxPredict, function(P) {
        (length(intersect(G, P))^2) / (length(P) * length(G))
      })
    }, mc.cores = num_cores)

  prPR <-
    matrix(
      unlist(lstPR),
      byrow = TRUE,
      ncol = length(cpxPredict)
    )
  mPrPRG <-
    apply(prPR, 1, max)
  PRG <-
    sum(mPrPRG * sizeGS) / sum(sizeGS)
  mPrPRP <-
    apply(prPR, 2, max)
  PRP <-
    sum(mPrPRP * sizePredict) / sum(sizePredict)

  PR <-
    2 * PRG * PRP /(PRG + PRP)
  return(
    c(
      "Acc" = Acc,
      "Jaccd" = Jaccd,
      "PR" = PR
    )
  )
}
