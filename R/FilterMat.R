FilterMat <- function(mat, n_fracs = 2){
  mat[is.na(mat)] <- 0
  fracPrt <-
    apply(mat, 1, function(x){
      y <-
        ifelse(x == 0, 0, 1)
      return(sum(y))
    })
  fMat <-
    mat[fracPrt >= n_fracs, ]
  return(fMat)
}
