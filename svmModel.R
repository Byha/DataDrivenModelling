# TODO: Add comment
# 
# Author: Byman Hamududu 22. okt. 2015 Run all selected DDM models.R
###############################################################################

svm.model<-function(traindata,testdata){
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
  
  ctrl <- trainControl(method = "repeatedcv",
                       repeats = 3,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)
  
  
  set.seed(1)
  ptm <- proc.time()
  tune.svm = tune(svm, Q ~ ., data = training, kernel = "radial",
                  ranges = list(cost = c(0.01, 0.1, 1, 5, 10)),
                  tunecontrol = tune.control(sampling = "boot"))
  proc.time() - ptm
  
  svm.pred <- predict(tune.svm$best.model, testing[1:test.len,])
  
#   svm.ev <- as.data.frame(cbind(cor(testQ, svm.pred),
#                                 rmse(testQ, svm.pred), #hydroGOF
#                                 NSE(testQ, svm.pred), #hydrGOF
#                                 VE(testQ, svm.pred), #topmodel
#                                 KGE(testQ, svm.pred[, 1]))) #hydroGOF
  
#   predicted.svm <- rbind(cbind(trainDates, tune.svm$best.model$fitted),
#                          cbind(testDates, svm.pred))
  #the best model
  
  #zoo.svm <- zoo(predicted.svm[, 2], as.Date(predicted.svm[, 1], origin = "1970-01-01"))
  save(tune.svm, file = paste(file.path(paste(getwd(), resultDirName, sep = "/"), modelsDirName), "/", "svm_", stName, ".RData", sep = ""))
} 