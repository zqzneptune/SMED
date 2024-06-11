PlotROCobjs <- function(
    listObjs, 
    sn, 
    colorplatte = 
      RColorBrewer::brewer.pal(8, "Dark2"), 
    txtX = 0.55, 
    txtY = 0.60
  ){
  library(pROC)
  numROC <-
    length(listObjs)
  a <- 0
  for(i in seq(numROC)){
    a <- a + 1
    if(i == 1){
      plot.roc(smooth(listObjs[[1]]),
               legacy.axes = TRUE,
               identity = FALSE,
               xlim = c(1, 0),
               xlab = "FPR(1 - specificity)", 
               ylab = "TPR(sensitivity)", 
               main = sn,
               col = colorplatte[i])
      text(txtX, txtY, adj = c(0,1), "Sample (AUC)")
    }else{
      lines.roc(smooth(listObjs[[i]]), col = colorplatte[i])
    }
    text(txtX, 
         txtY - 0.07*a, 
         adj = c(0, 1),
         paste0(names(listObjs)[i], ": ", sprintf("%.3f", auc(listObjs[[i]]))), 
         col = colorplatte[i] )
  }
}