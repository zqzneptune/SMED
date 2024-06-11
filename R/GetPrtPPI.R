GetPrtPPI <- function(prt){
  prt <-
    sort(prt)
  s <-
    data.frame(RcppAlgos::comboGeneral(prt, 2))
  colnames(s) <-
    c("InteractorA", "InteractorB")
  s[, "PPI"] <-
    paste(s[, 1], s[, 2], sep = "~")
  return(s)
}
