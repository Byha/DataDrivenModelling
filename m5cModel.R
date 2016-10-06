# TODO: Add comment
# M5c (‘c’ - ‘cubist’) generates M5 model trees using the M5’ algorithm, and  'c' enhances the original M5 algorithm
# Author: Byman Hamududu 22. okt. 2015 Running m5c.model.R
###############################################################################

m5c.model<-function(traindata,testdata){
  library(e1071);
  library(nnet)
  library(caret);
  library(topmodel);
  library(hydroGOF);
  library(C50);
  library(RSNNS);
  library(RWeka)
  library(plotrix)
  library(openair)
  library(gbm)
  library(Hmisc)
  library(Cubist)
  
  test.len <- dim(testing)[1]
  
  training <- training[, -1]
  
  Q <- training$Q
  xtraining <- training[, -9]
  set.seed(1)
  ptm <- proc.time()
  cTune <- train(x = xtraining, y = Q,
                 "cubist", tuneGrid = expand.grid(.committees = c(1, 10, 50,  100),
                                                  .neighbors = c(0, 1, 5, 9)), trControl = trainControl(method = "cv"),pred=TRUE)
  #  print(cTune)
  proc.time() - ptm
  
#   m5c.pred <- predict(cTune, testing[1:test.len, ], neighbors = 5)
#   #
#   
#   m5c.ev <- as.data.frame(cbind(cor(testQ, m5c.pred),
#                                 rmse(testQ, m5c.pred), #hydroGOF
#                                 NSE(testQ, m5c.pred), #hydrGOF
#                                 VE(testQ, m5c.pred), #topmodel
#                                 KGE(testQ, m5c.pred))) #hydroGOF
#   
#   predicted.m5c <- rbind(cbind(trainDates, cTune$trainingData$.outcome),
#                          cbind(testDates, m5c.pred))
#   #the best model
#   
#   zoo.m5c <- zoo(predicted.m5c[, 2], as.Date(as.numeric(as.character(predicted.m5c[, 1])), origin = "1970-01-01"))
  save(cTune, file = paste(file.path(paste(getwd(), resultDirName, sep = "/"), modelsDirName), "/", "m5c_", stName, ".RData", sep = ""))
} 