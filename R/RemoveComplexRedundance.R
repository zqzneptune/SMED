RemoveComplexRedundance <- function(rawCpx){
  jd <-
    lapply(rawCpx, function(x){
      lapply(rawCpx, function(y){
        return((length(intersect(x, y)))/(length(union(x, y)))) # Jaccard Index
      })
    })
  jm <-
    matrix(unlist(jd), byrow = TRUE, ncol = length(jd))
  jtree <-
    hclust(dist(jm, method = "euclidean"), method = "ward.D2")
  jtree$height <-
    round(jtree$height, 6)
  jbranch <-
    cutree(jtree, h = 0.2)
  names(rawCpx) <-
    paste("CID", jbranch, sep = "_")
  refCpx <-
    lapply(unstack(stack(rawCpx)), function(x){
      return(unique(x))
    })
  return(refCpx)
}