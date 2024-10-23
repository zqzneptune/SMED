GetClusterStats <- function(Cpx){
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
        sprintf("%.2f%%", NumDimers/NumCpx*100),
      "PctTrimers" =
        sprintf("%.2f%%", NumTrimers/NumCpx*100),
      "PctLarge" =
        sprintf("%.2f%%", NumLarge/NumCpx*100),
      "AvgCpxSize" =
        sprintf("%.2f", AvgCpxSize)
    )
  )
}
