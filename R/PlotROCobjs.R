#' Plot Multiple ROC Curves
#'
#' This function plots multiple ROC curves on a single plot for comparison.
#'
#' @param listObjs A list of ROC objects (from `pROC::roc`).
#' @param sn Character title for the plot.
#' @param colorplatte Character vector of colors for the curves.
#' @param txtX x-coordinate for the legend text.
#' @param txtY y-coordinate for the legend text.
#'
#' @return None (invokes a plot).
#' @importFrom pROC smooth plot.roc lines.roc auc
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics text
#' @export
PlotROCobjs <- function(
    listObjs, 
    sn, 
    colorplatte = 
      RColorBrewer::brewer.pal(8, "Dark2"), 
    txtX = 0.55, 
    txtY = 0.60
  ){
  numROC <-
    length(listObjs)
  a <- 0
  for(i in seq(numROC)){
    a <- a + 1
    if(i == 1){
      pROC::plot.roc(pROC::smooth(listObjs[[1]]),
               legacy.axes = TRUE,
               identity = FALSE,
               xlim = c(1, 0),
               xlab = "FPR(1 - specificity)", 
               ylab = "TPR(sensitivity)", 
               main = sn,
               col = colorplatte[i])
      graphics::text(txtX, txtY, adj = c(0,1), "Sample (AUC)")
    }else{
      pROC::lines.roc(pROC::smooth(listObjs[[i]]), col = colorplatte[i])
    }
    graphics::text(txtX, 
         txtY - 0.07*a, 
         adj = c(0, 1),
         paste0(names(listObjs)[i], ": ", sprintf("%.3f", pROC::auc(listObjs[[i]]))), 
         col = colorplatte[i] )
  }
}