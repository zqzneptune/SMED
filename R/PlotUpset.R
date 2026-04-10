#' Plot PPI Overlap using UpSet Plot
#'
#' This function visualizes the overlap between different sets of protein-protein
#' interactions using an UpSet plot.
#'
#' @param listTerms A named list where each element is a character vector of
#'   PPI strings (e.g., "ProtA~ProtB").
#' @param n_interactions Maximum number of intersections to show. Default is 20.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom ggupset scale_x_upset
#' @importFrom magrittr %>%
#' @importFrom utils stack unstack
#' @export
PlotUpset <- function(listTerms, n_interactions = 20){
  # input list format example:
  # list(`A` = c("AA", "BB", "CC"), `B` = c("BB", "CC", "DD", "EE"))
  
  rawTerms <-
    utils::unstack(utils::stack(listTerms)[, c("ind", "values")])
  datItem <-
    data.frame(`Item` = names(rawTerms))
  datItem$Term <-
    rawTerms
  
  p <-
    ggplot2::ggplot(datItem, ggplot2::aes(x = `Term`)) +
    ggplot2::geom_bar() +
    ggplot2::geom_text(stat = "count", ggplot2::aes(label = ggplot2::after_stat(count)), vjust = -1) +
    ggupset::scale_x_upset(n_intersections = n_interactions)
  
  return(p) 
}