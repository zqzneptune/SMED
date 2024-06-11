ScorePrInCE <- function(rawMat, goldstandard, top_ppi = 20000){
  mat <-
    rawMat[sort(rownames(rawMat)), ]
  
  rawPPI <-
    PrInCE::PrInCE(
      profiles = mat,
      gold_standard = goldstandard
    )
  s <- 
    apply(rawPPI[, c("protein_A", "protein_B")], 1, sort)
  rawPPI[, "PPI"] <-
    paste(s[1, ], s[2, ], sep = "~")
  rawPPI[, "PrInCE"] <-
    rawPPI$score
  
  finalPPI <-
    rawPPI[rev(order(rawPPI$PrInCE)), ]
  
  if(nrow(finalPPI) > top_ppi){
    datPPI <-
      finalPPI[1:top_ppi, ]
  }else{
    datPPI <-
      finalPPI
  }
  return(datPPI[, c("PPI", "PrInCE")])
}
