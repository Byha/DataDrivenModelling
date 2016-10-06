# TODO: Add comment
#  M5 (sometimes with‘P’ - ‘prime’) generates M5 model trees using the M5’ algorithm, and  'P' enhances the original M5 algorithm
#
# Author: Byman Hamududu 22. okt. 2015 Running  M5 models.R
###############################################################################

m5n.model<-function(traindata,testdata){
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
  
  rctrl <- trainControl(method = "cv", number = 3, returnResamp = "all")
  set.seed(1)
  ptm <- proc.time()
  #m5 <- M5P(Q ~ ., data = training)
  
  M5n <- caret::train(Q ~ ., data = training, method = "M5", trControl = rctrl,
               preProc = c("center", "scale"))
  proc.time() - ptm
  
#   m5.pred <- predict(M5n, testing[1:test.len,])
#   
#   m5.ev <- as.data.frame(cbind(cor(testQ, m5.pred),
#                                rmse(testQ, m5.pred), #hydroGOF
#                                NSE(testQ, m5.pred), #hydrGOF
#                                VE(testQ, m5.pred), #topmodel
#                                KGE(testQ, m5.pred))) #hydroGOF
#   
#   predicted.m5 <- rbind(cbind(trainDates, M5n$finalModel$predictions ),
#                         cbind(testDates, m5.pred))
#   
#   
  zoo.m5 <- zoo(predicted.m5[, 2], as.Date(predicted.m5[, 1], origin = "1970-01-01"))
  save(M5n, file = paste(file.path(paste(getwd(), resultDirName, sep = "/"), modelsDirName), "/", "m5n_", stName, ".RData", sep = ""))
} 