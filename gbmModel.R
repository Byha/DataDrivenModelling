# TODO: Add comment
# Generalized Boosting  Models
# Author: Byman Hamududu 22. okt. 2015  Running gbm.model.R
###############################################################################

gbm.model<-function(traindata,testdata){
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
  
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  my.grid <- expand.grid(.decay = c(0, 0.01, 0.1), .size = c(1, 5, 9))
  grid <- expand.grid(size = c(5, 10, 20, 50), k = c(1, 2, 3, 4, 5))
  
  set.seed(7)
  ptm <- proc.time()
  modelGbm <- train(Q ~ ., data = training, method = "gbm", trControl = control, verbose = FALSE)
  proc.time() - ptm
  
#   gbm.pred <- predict(modelGbm, testing[1:test.len,])
#   
#   
#   gbm.ev <- as.data.frame(cbind(cor(testQ, gbm.pred),
#                                 rmse(testQ, gbm.pred), #hydroGOF
#                                 NSE(testQ, gbm.pred), #hydrGOF
#                                 VE(testQ, gbm.pred), #topmodel
#                                 KGE(testQ, gbm.pred))) #hydroGOF
#   
#   predicted.gbm <- rbind(cbind(trainDates, modelGbm$finalModel$fit),
#                          cbind(testDates, gbm.pred))
#   #the best model
#   
#   zoo.gbm <- zoo(predicted.gbm[, 2], as.Date(predicted.gbm[, 1], origin = "1970-01-01"))
  
  save(modelGbm, file = paste(file.path(paste(getwd(), resultDirName, sep = "/"), modelsDirName), "/", "gbm_", stName, ".RData", sep = ""))
} 