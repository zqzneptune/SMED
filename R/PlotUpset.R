PlotUpset <- function(listTerms, n_interactions = 20){
  # input list format example:
  # list(`A` = c("AA", "BB", "CC"), `B` = c("BB", "CC", "DD", "EE"))
  suppressPackageStartupMessages(library(ggupset))
  suppressPackageStartupMessages(library(ggplot2))
  rawTerms <-
    unstack(stack(listTerms)[, c("ind", "values")])
  datItem <-
    data.frame(`Item` = names(rawTerms))
  datItem$Term <-
    rawTerms
  
  p <-
    datItem %>%
      ggplot(aes(x = `Term`)) +
      geom_bar() +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
      scale_x_upset(n_intersections = n_interactions)
  
  return(p) 
}