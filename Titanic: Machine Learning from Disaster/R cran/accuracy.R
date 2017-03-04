evaluateAccuracy <- function(sol_path)
{
  library('compare')
  trainfile <- read.csv('../input/train.csv', stringsAsFactors = F)
  solutioncheck <- read.csv(sol_path, stringsAsFactors = F)
  
  traincheck <- data.frame(PassengerID = trainfile$PassengerId, Survived = trainfile$Survived)
  
  # check data
  success_values = c('0','2')
  compare <- data.frame(PassengerID = trainfile$PassengerId, Survived = traincheck$Survived + solutioncheck$Survived)
  success <- compare[compare$Survived %in% success_values,]
  accuracy = nrow(success) / nrow(traincheck)
  return(accuracy)
}