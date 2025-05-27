#' Remove Complex Redundancy via Hierarchical Clustering
#'
#' Reduces redundancy in a list of protein complexes. It calculates pairwise
#' Jaccard indices between all complexes, performs hierarchical clustering on
#' Euclidean distances of these Jaccard similarity profiles,
#' and then merges complexes that fall into the same cluster by uniting their
#' protein members.
#'
#' @param raw_complex_list A list where each element is a character vector
#'   of protein identifiers representing a complex.
#' @param hclust_method The agglomeration method to be used for hierarchical
#'   clustering. See `hclust` (e.g., "ward.D2", "complete"). Default "ward.D2".
#' @param cut_height Numeric, the height at which to cut the dendrogram to
#'   form clusters of complexes. Complexes within the same cluster will be
#'   merged. Default 0.2. This threshold applies to the heights derived from
#'   Euclidean distances of Jaccard similarity profiles.
#'
#' @return A list of refined complexes. Each element is a character vector of
#'   unique protein identifiers, and names are generated based on cluster IDs
#'   (e.g., "CID_1").
#' @export
#' @importFrom stats hclust cutree
#' @importFrom proxy dist
#' @examples
#' cplx_list <- list(
#'   c1 = c("A", "B", "C"),
#'   c2 = c("B", "C", "D"), # Similar to c1
#'   c3 = c("C", "D", "E"), # Similar to c2
#'   c4 = c("F", "G"),      # Different
#'   c5 = c("A", "B", "C", "D") # Superset of c1 and c2
#' )
#' if (requireNamespace("proxy", quietly = TRUE)) {
#'   refined_cplx <- remove_complex_redundancy(cplx_list, cut_height = 0.5)
#'   # print(refined_cplx)
#' }
remove_complex_redundancy <- function(raw_complex_list,
                                      hclust_method = "ward.D2",
                                      cut_height = 0.2) {
  if (!is.list(raw_complex_list) || length(raw_complex_list) == 0) {
    return(list())
  }
  if (length(raw_complex_list) == 1) {
    names(raw_complex_list) <- "CID_1"
    return(lapply(raw_complex_list, unique))
  }

  num_complexes <- length(raw_complex_list)

  # Calculate pairwise Jaccard Index matrix
  jaccard_matrix <- matrix(0, nrow = num_complexes, ncol = num_complexes)
  for (i in seq_len(num_complexes)) {
    for (j in seq_i(i, num_complexes)) { # Efficiently fill matrix
      proteins_i <- unique(raw_complex_list[[i]])
      proteins_j <- unique(raw_complex_list[[j]])
      intersection_size <- length(intersect(proteins_i, proteins_j))
      union_size <- length(union(proteins_i, proteins_j))
      if (union_size == 0) { # Both empty or identical empty
        j_index <- 1.0
      } else {
        j_index <- intersection_size / union_size
      }
      jaccard_matrix[i, j] <- j_index
      jaccard_matrix[j, i] <- j_index
    }
  }
  # seq_i helper for upper/lower triangle
  seq_i <- function(from, to) if (from > to) integer(0) else seq.int(from, to)


  # Hierarchical clustering based on Jaccard similarity profiles
  # Using proxy::dist for Euclidean distance.
  # Note: `proxy` typically uses capitalized method names, e.g., "Euclidean".
  if (nrow(jaccard_matrix) < 2) { # Cannot cluster a single item
    if (nrow(jaccard_matrix) == 1) {
      names(raw_complex_list) <- "CID_1"
      return(lapply(raw_complex_list, unique))
    } else { # 0 complexes
      return(list())
    }
  }

  # Using proxy::dist instead of stats::dist
  # The result of proxy::dist is a 'dist' object, compatible with stats::hclust
  distance_matrix <- proxy::dist(jaccard_matrix, method = "Euclidean")
  hc_tree <- stats::hclust(distance_matrix, method = hclust_method)

  # Round heights to avoid floating point precision issues in cutree
  hc_tree$height <- round(hc_tree$height, 6)
  complex_clusters <- stats::cutree(hc_tree, h = cut_height)

  # Merge complexes within each cluster
  unique_cluster_ids <- unique(complex_clusters)
  refined_complex_list <- lapply(unique_cluster_ids, function(cluster_id) {
    indices_in_cluster <- which(complex_clusters == cluster_id)
    # Combine all proteins from complexes in this cluster
    proteins_from_cluster_complexes <-
      unlist(raw_complex_list[indices_in_cluster])
    # Return unique, sorted proteins for the merged complex
    return(sort(unique(proteins_from_cluster_complexes)))
  })

  names(refined_complex_list) <- paste0("CID_", unique_cluster_ids)
  return(refined_complex_list)
}
