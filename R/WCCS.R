WCCS <- function(e){
  eList <-
    as.list(as.data.frame(t(e)))
  eListPair <-
    combn(eList, 2, simplify = FALSE)
  eWCC <-
    lapply(eListPair, function(x){
      wccsom::wcc(x[[1]], x[[2]], trwdth = 1)
    })
  WCC <- 
    unlist(eWCC)
  WCC[is.na(WCC)] <- 0
  return(WCC)
}