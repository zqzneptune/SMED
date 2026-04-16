#' Plot Multiple ROC Curves
#'
#' This function plots multiple ROC curves on a single plot for comparison using ggplot2.
#'
#' @param listObjs A list of ROC objects (from `pROC::roc`).
#' @param sn Character title for the plot.
#' @param colorplatte Character vector of colors for the curves.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom pROC coords auc
#' @export
PlotROCobjs <- function(
    listObjs, 
    sn, 
    colorplatte = RColorBrewer::brewer.pal(8, "Dark2")
  ){
  
  plot_data <- list()
  auc_labels <- c()
  
  for(i in seq_along(listObjs)){
    roc_obj <- listObjs[[i]]
    roc_name <- names(listObjs)[i]
    if(is.null(roc_name)) roc_name <- paste0("Model_", i)
    
    # Extract coordinates
    coords_df <- as.data.frame(pROC::coords(roc_obj, "all", transpose = FALSE))
    coords_df$Model <- paste0(roc_name, " (AUC: ", sprintf("%.3f", pROC::auc(roc_obj)), ")")
    plot_data[[i]] <- coords_df
  }
  
  combined_data <- do.call(rbind, plot_data)
  
  p <- ggplot2::ggplot(combined_data, ggplot2::aes(x = 1 - specificity, y = sensitivity, color = Model)) +
    ggplot2::geom_path(linewidth = 1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    ggplot2::scale_color_manual(values = colorplatte) +
    ggplot2::labs(
      title = sn,
      x = "FPR (1 - Specificity)",
      y = "TPR (Sensitivity)",
      color = "Sample (AUC)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = c(0.7, 0.3),
      legend.background = ggplot2::element_rect(fill = "white", color = "black"),
      text = ggplot2::element_text(size = 12)
    )
    
  return(p)
}