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
