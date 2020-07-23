MachineLearning <- function(rawScore, refIntTrain, fnMachine){
  # fnMachine <-
  #   c("adaboost", "earth", "rf", "svmRadial",
  #     "LogitBoost", "kknn", "gbm")
  # library(pROC)
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
  ### Imputation
  datTrain <-
    apply(datScore, 2, function(x){
      impute(x)
    })
  preProcess_missingdata_model <-
    preProcess(datTrain)
  # preProcess_missingdata_model
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
  # summary(as.factor(trainData$Response))
  # set.seed(998)
  # inTraining <-
  #   #  sample(seq_len(nrow(trainData)), 20000)
  #   createDataPartition(trainData$Response, p = .1, list = FALSE)
  training <-
    trainData#[inTraining, ]
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
  # listAUCs <-
  #   lapply(listPred, function(sc){
  #     suppressMessages(objs <- roc(rawResponsce, sc, na.rm = TRUE))
  #     return(auc(objs))
  #   })
  datPred <-
    do.call(cbind, listPred)
  avePred <-
    rowMeans(datPred)
  # pAUC <-
  #   roc(rawResponsce, avePred, na.rm = TRUE)
  datScore <-
    cbind(rawScore, datPred)
  datScore[, "SMED"] <-
    avePred
}
