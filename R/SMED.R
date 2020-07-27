SMED <- function(mRaw, trainInt, fnMachine){
  # trainInt <-
  #   SMED::GetTrainPPI(cpxGS)
  rawScore <-
    SMED::EultionScore(mRaw)
  datSMED <-
    SMED::MachineLearning(rawScore = rawScore,
                          refIntTrain = trainInt,
                          fnMachine = fnMachine)
  return(datSMED)
}
