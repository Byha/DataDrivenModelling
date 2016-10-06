# TODO: Add comment
# 
# Author: Byman Hamududu 22. okt. 2015 Run all selected DDM models.R
###############################################################################

nnet.model<-function(traindata,testdata){
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
  
  
  my.grid <- expand.grid(.decay = c(0, 0.01, 0.1, 10), .size = c(1, 5, 9))
  
  test.len <- dim(testing)[1]
  
  training <- training[, -1]
  
  ptm <- proc.time()
  tune.nnet <- tune.nnet(Q ~ ., data = training, size = c(1, 5, 9), decay = NULL, maxit = 1000, tuneGrid = my.grid,
                         linout = 1, trace = FALSE, tunecontrol = tune.control(sampling = "boot"))
  proc.time() - ptm
  
#   testing <- testing[, 2:9]
#   nnet.pred <- predict(tune.nnet$best.model, newdata = testing[1:test.len,])
#   
#   nnet.ev <- as.data.frame(cbind(cor(testQ, nnet.pred[, 1]),
#                                  rmse(testQ, nnet.pred[, 1]),
#                                  NSE(testQ, nnet.pred[, 1]),
#                                  VE(testQ, nnet.pred[, 1]),
#                                  KGE(testQ, nnet.pred[, 1])))
#   # idealpoint(testQ,nnet.pred[, 1])
#   # merging hbv and nnet evaluations and writing them to file
#   
#   # predicted <- rbind(cbind(trainDates, tune.nnet$best.model$fitted.value), cbind(testDates, nnet.pred))
#   predicted.nnet <- rbind(cbind(trainDates, tune.nnet$best.model$fitted.value),
#                           cbind(testDates, nnet.pred))
#   #the best model
#   
#     zoo.nnet <- zoo(predicted.nnet[, 2], as.Date(predicted.nnet[, 1], origin = "1970-01-01"))
    
    save(tune.nnet, file = paste(file.path(paste(getwd(), resultDirName, sep = "/"), modelsDirName), "/", "nnet_", stName, ".RData", sep = ""))
} 