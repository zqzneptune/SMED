MachineLearning <- function(rawScore, refIntTrain, fnMachine){
  library(Hmisc)
  library(RcppAlgos)
  library(caret)
  library(caretEnsemble)
  rawPPI <-
    rawScore$PPI
  rawResponsce <-
    ifelse(rawPPI %in% refIntTrain$TP$PPI, 1,
           ifelse(rawPPI %in% refIntTrain$TN$PPI, 0, NA))
  datScore <-
    rawScore[, -1]
  datTrain <-
    apply(datScore, 2, function(x){
      impute(x)
    })
  preProcess_missingdata_model <-
    preProcess(datTrain)
  prepData <-
    predict(preProcess_missingdata_model, newdata = datTrain)
  trainData <-
    data.frame(`PPI` = rawPPI,
               prepData,
               stringsAsFactors = FALSE)
  trainData[, "Response"] <-
    ifelse(trainData$PPI %in% refIntTrain$TP$PPI, "Y",
           ifelse(trainData$PPI %in% refIntTrain$TN$PPI, "N", NA))
  trainData <-
    trainData[!is.na(trainData$Response), -1]
  training <-
    trainData
  summary(as.factor(training$Response))
  set.seed(100)
  fitControl <-
    trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 10,
      savePredictions = "final",       # saves predictions for optimal tuning parameter
      classProbs = TRUE,                 # should class probabilities be returned
      summaryFunction = twoClassSummary  # results summary function
    )
  listPred <-
     list()
  for(fnM in fnMachine){
    message(fnM)
    set.seed(100)
    suppressMessages(models <-
                       train(`Response` ~ .,
                             data = training,
                             method = fnM,
                             trControl = fitControl))
    preds <-
      predict(models, newdata = prepData, type = "prob")
    rm(models)
    listPred[[fnM]] <-
      preds[, "Y"]
  }
  datPred <-
    do.call(cbind, listPred)
  avePred <-
    rowMeans(datPred)
  datScore <-
    cbind(rawScore, datPred)
  datScore[, "SMED"] <-
    avePred
  return(datScore)
}
