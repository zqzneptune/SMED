#' Remove Redundant Protein Complexes
#'
#' This function merges redundant protein complexes in a list based on their
#' Jaccard similarity and hierarchical clustering.
#'
#' @param rawCpx A list of character vectors, each representing a protein complex.
#'
#' @return A list of unique protein complexes with renamed IDs.
#' @importFrom stats dist hclust cutree
#' @importFrom utils stack unstack
#' @export
RemoveComplexRedundance <- function(rawCpx){
  dt <- utils::stack(rawCpx)
  proteins <- unique(dt$values)
  complexes <- names(rawCpx)
  
  incidence_matrix <- table(factor(dt$values, levels = proteins), factor(dt$ind, levels = complexes))
  
  intersections <- crossprod(incidence_matrix)
  size <- colSums(incidence_matrix)
  
  union_size <- outer(size, size, FUN = "+") - intersections
  jaccard_sim <- intersections / union_size
  
  jm <- as.matrix(jaccard_sim)
  jtree <-
    stats::hclust(stats::dist(jm, method = "euclidean"), method = "ward.D2")
  jtree$height <-
    round(jtree$height, 6)
  jbranch <-
    stats::cutree(jtree, h = 0.2)
  names(rawCpx) <-
    paste("CID", jbranch, sep = "_")
  refCpx <-
    lapply(utils::unstack(utils::stack(rawCpx)), function(x){
      return(unique(x))
    })
  return(refCpx)
}