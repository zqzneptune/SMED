SMED <- function(mRaw, trainInt, fnMachine){
  rawScore <-
    SMED::EultionScore(mRaw)
  datSMED <-
    SMED::MachineLearning(rawScore = rawScore,
                          refIntTrain = trainInt,
                          fnMachine = fnMachine)
  return(datSMED)
}
