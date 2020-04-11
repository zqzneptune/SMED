SMED <- function(datInput){
  datCnt <-
    datInput %>%
    mutate(`NormalSpec` = `countPrey`/`lenPrey`) %>%
    group_by(`idRun`) %>%
    mutate(`SumNS` = sum(`NormalSpec`)) %>%
    mutate(`NSAF` = `NormalSpec`/`SumNS`) %>%
    group_by(`idRun`) %>%
    mutate(`NormalNSAF` = `NSAF`/min(`NSAF`)) %>%
    mutate(`Tn` = as.integer(sqrt(`NormalNSAF`)))

  d <- tidyr::spread(datCnt[, c("idRun", "idPrey", "Tn")],
                     `idRun`, `Tn`)
  pps <-
    RcppAlgos::comboGeneral(d$idPrey, 2)
  f <-
    t(as.matrix(d[, -1]))
  f[!is.na(f)] <- 1
  f[is.na(f)] <- 0

  DICE <- .GetDICE(f)

  g <-
    t(as.matrix(d[, -1]))
  g[is.na(g)] <- 0

  MinTN <- .GetMinTN(g)
  rawPPI <-
    data.frame(pps,
               DICE[lower.tri(DICE, diag = FALSE)],
               MinTN[lower.tri(MinTN, diag = FALSE)],
               stringsAsFactors = FALSE)
  colnames(rawPPI) <-
    c("InteractorA", "InteractorB", "DICE", "ppiTN")
  datPPI <-
    rawPPI[rawPPI$ppiTN != 0, ]

  datPPI$ppiTN <-
    as.numeric(datPPI$ppiTN)
  tnInteractorA <-
    datPPI[, c("InteractorA", "ppiTN")]
  colnames(tnInteractorA) <-
    c("UniprotID", "ppiTN")
  tnInteractorB <-
    datPPI[, c("InteractorB", "ppiTN")]
  colnames(tnInteractorB) <-
    c("UniprotID", "ppiTN")
  tnProtein <-
    bind_rows(tnInteractorA, tnInteractorB) %>%
    group_by(`UniprotID`) %>%
    summarise(minTn = sum(`ppiTN`))
  sumMinTnInteractorA <-
    tnProtein
  colnames(sumMinTnInteractorA) <-
    c("InteractorA", "tnA")
  sumMinTnInteractorB <-
    tnProtein
  colnames(sumMinTnInteractorB) <-
    c("InteractorB", "tnB")
  scorePPI <-
    datPPI %>%
    left_join(., sumMinTnInteractorA, by = "InteractorA") %>%
    left_join(., sumMinTnInteractorB, by = "InteractorB") %>%
    mutate(`NMinTn` = sum(tnProtein$minTn)/2) %>%
    mutate(`rawHyper` = -phyper(`ppiTN`, `tnA`, `NMinTn` - `tnB`,
                                `tnB`, lower.tail = FALSE, log.p = TRUE)) %>%
    mutate(`SMED` = `DICE` * `rawHyper`) %>%
    mutate(`PPI` = paste(`InteractorA`, `InteractorB`, sep = "~"))
  return(scorePPI)

}
