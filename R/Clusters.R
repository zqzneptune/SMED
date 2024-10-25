StatsCpx <- function(Cpx){
  NumCpx <-
    length(Cpx)
  NumSubunit <-
    unlist(lapply(Cpx, length))
  SumNumSubunit <-
    table(NumSubunit)
  AvgCpxSize <-
    mean(NumSubunit)
  NumDimers <-
    ifelse(is.na(unname(SumNumSubunit["2"])), 0, unname(SumNumSubunit["2"]))
  NumTrimers <-
    ifelse(is.na(unname(SumNumSubunit["3"])), 0, unname(SumNumSubunit["3"]))
  NumLarge <-
    sum(SumNumSubunit[as.numeric(names(SumNumSubunit)) > 10])
  return(
    c(
      "NumClusters" =
        NumCpx,
      "NumSubunits" =
        length(unique(unlist(Cpx))),
      "PctDimers" =
        NumDimers/NumCpx*100,
      "PctTrimers" =
        NumTrimers/NumCpx*100,
      "PctLarge" =
        NumLarge/NumCpx*100,
      "AvgCpxSize" =
        AvgCpxSize
    )
  )
}


# Citation: Zhang X-F, Dai D-Q, Ou-Yang L, Wu M-Y (2012) Exploring Overlapping Functional Units with Various Structure in Protein Interaction Networks. PLoS ONE 7(8): e43092. https://doi.org/10.1371/journal.pone.0043092
# Source: Text S2. https://doi.org/10.1371/journal.pone.0043092.s005
EvaCpx <- function(cpxG, cpxC){
  sizeG <-
    unlist(lapply(cpxG, length))
  sizeG <-
    unname(sizeG)
  sizeC <-
    unlist(lapply(cpxC, length))
  sizeC <-
    unname(sizeC)
  sizePvsGS <-
    unlist(lapply(cpxC, function(P){
      length(intersect(P, unique(unlist(cpxG))))
    }))
  sizePvsGS <-
    unname(sizePvsGS)
  if(sum(sizePvsGS) == 0){
    message("No overlap detected.")
    return(NULL)
  }else{
    prT <-
      matrix(unlist(lapply(cpxG, function(G){
        lapply(cpxC, function(P){
          length(intersect(G, P))
        })
      })), byrow = TRUE, ncol = length(cpxC))

    mPrT <-
      apply(prT, 1, max)
    Sn <-
      sum(mPrT)/sum(sizeG)

    nPrT <-
      apply(prT, 2, max)
    PPV <-
      sum(nPrT)/sum(sizePvsGS)

    prJac <-
      matrix(unlist(lapply(cpxG, function(G){
        lapply(cpxC, function(P){
          length(intersect(G, P))/length(union(G, P))
        })
      })), byrow = TRUE, ncol = length(cpxC))

    mJacG <-
      apply(prJac, 1, max)
    jacG <-
      sum(mJacG * sizeG) / sum(sizeG)
    mJacP <-
      apply(prJac, 2, max)
    jacP <-
      sum(mJacP * sizeC) / sum(sizeC)
    Jaccd <-
      2 * jacP * jacG /(jacP + jacG)

    prPR <-
      matrix(unlist(lapply(cpxG, function(G){
        lapply(cpxC, function(P){
          length(intersect(G, P))^2/length(P)/length(G)
        })
      })), byrow = TRUE, ncol = length(cpxC))
    mPrPRG <-
      apply(prPR, 1, max)
    PRG <-
      sum(mPrPRG * sizeG) / sum(sizeG)
    mPrPRP <-
      apply(prPR, 2, max)
    PRP <-
      sum(mPrPRP * sizeC) / sum(sizeC)
    PR <-
      2 * PRG * PRP /(PRG + PRP)
    return(
      c(
        "ACC" = sqrt(Sn * PPV),
        "Jaccd" = Jaccd,
        "PR" = PR
      )
    )
  }

}
