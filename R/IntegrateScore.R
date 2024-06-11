IntegrateScore <- function(rawScore, rawResponse, fnM){
  message(fnM)
  library(doParallel)
  library(Hmisc)
  library(RcppAlgos)
  library(caret)
  library(caretEnsemble)
  library(pROC)
  datTrain <-
    apply(rawScore, 2, function(x){
      impute(x)
    })
  
  preProcess_missingdata_model <-
    preProcess(datTrain)
  
  prepData <-
    predict(preProcess_missingdata_model, newdata = datTrain)
  
  trainData <-
    data.frame(prepData)
  
  trainData[, "Response"] <-
    rawResponse
  
  training <-
    trainData[!is.na(trainData$Response), -1]
  
  # summary(as.factor(training$Response))
  set.seed(100)
  fitControl <-
    trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 1,
      savePredictions = "final", # saves predictions for optimal tuning parameter
      classProbs = TRUE, # should class probabilities be returned
      summaryFunction = twoClassSummary  # results summary function
    )
  
  
  set.seed(100)
  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)
  suppressMessages(models <-
                     train(`Response` ~ .,
                           data = training,
                           method = fnM,
                           trControl = fitControl))
  stopCluster(cl)
  
  preds <-
    predict(models, newdata = prepData, type = "prob")
  rm(models)
  return(preds[, "Y"])
}