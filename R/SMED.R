SMED <- function(mRaw, trainInt){
  fnMachine <-
    c("adaboost", "earth", "rf", "svmRadial",
      "LogitBoost", "kknn", "gbm")
  rawScore <-
    SMED::EultionScore(mRaw)
  datSMED <-
    SMED::MachineLearning(rawScore = rawScore,
                          refIntTrain = trainInt,
                          fnMachine = fnMachine)
  return(datSMED)
}
