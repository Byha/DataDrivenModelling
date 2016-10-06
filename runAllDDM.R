# TODO: Add comment
# This function will call the indidvidual machine learning model, training and test, and write results including the performance
#
# Author: Byman Hamududu 22. okt. 2015 Running  all  models.R
###############################################################################
library(e1071);
library(nnet)
library(caret);
library(topmodel);
library(hydroGOF);
library(pROC);
library(C50);
library(RSNNS);
library(xlsx)
library(RWeka)
library(plotrix)
library(openair)
library(gbm)
library(Hmisc)
library(Cubist)

setwd("D:\\workspace\\DDM\\ML")
rm(list = ls())

source(file="D:/workspace/DDM/functions/m5nModel.R")
resultDirName<-"Results"         #Names of directory to save the result in
plotsDirName <- "Plots" #Names of directory to save the plots in
modelsDirName <- "Models" # Name of directory where to keep trained model 

dir.create(file.path(getwd(), resultDirName), showWarnings = FALSE)
dir.create(file.path(paste(getwd(), resultDirName, sep = "/"), plotsDirName), showWarnings = FALSE)
dir.create(file.path(paste(getwd(), resultDirName, sep = "/"), modelsDirName), showWarnings = FALSE)

####################################################

#LOAD UTILITY functions  normalize, etc..

sapply(list.files(pattern = "[.]R$", path = "D:\\workspace\\DDM\\FUNCTIONS", full.names = TRUE), source);

data.path <- 'D:/workspace/DDM/data'
filList <- list.files(path = data.path, pattern = 'hel.txt')

#TODO loop through all the data files for several measuring stations ----
# ================================================================


#-----------------------------RAW DATA -------------------------------------------------------
for (i in 2:length(filList)) {
  
  stName <- paste(strsplit(filList[i], "_")[[1]][1], capitalize(strsplit(filList[i], "_")[[1]][2]), sep = "_")
  `dat` <- read.table(file.path(data.path, filList[i]), quote = "\"", comment.char = "")
  
  dat$mydates <- as.Date(paste(dat$V1, dat$V2, dat$V3, sep = "-"))
  dat.z <- zoo(dat[, 6], dat$mydates)
  dat <- dat[, 4:7] # remove date columns
  dat.ts <- ts(dat, start = dat[1, 4], frequency = 365)
  
  # Lag the rainfall, temperature, Runoff data by 1 and 2 days
  dat_lag <- cbind(ts(dat[, 4]), ts(dat[, 1]),
                   lag(ts(dat[, 1]), k = -1),
                   lag(ts(dat[, 1]), k = -2),
                   ts(dat[, 2]),
                   lag(ts(dat[, 2]), k = -1),
                   lag(ts(dat[, 2]), k = -2),
                   
                   lag(ts(dat[, 3]), k = -1),
                   lag(ts(dat[, 3]), k = -2),
                   ts(dat[, 3]))
  
  
  dat_lag1 <- as.data.frame(dat_lag[3:(dim(dat_lag)[1] - 4),]) # Delete the first 3 lines with NA and the last 3 lines with NA 
  
  
  names(dat_lag1) <- c("Dates", "P", "Pl1d", "Pl2d", "T", "Tl1d", "Tl2d", "Ql1d", "Ql2d", "Q")    #add names to the data frame
  
  # reformat the matrix into data frame 
  dat1 <- as.data.frame(cbind(dat_lag1$Dates, dat_lag1$P, dat_lag1$Pl1d, dat_lag1$Pl2d, dat_lag1$T, dat_lag1$Tl1d, dat_lag1$Tl2d, dat_lag1$Ql1d, dat_lag1$Ql2d, dat_lag1$Q))
  
  names(dat1) <- c("Dates", "P", "Pl1d", "Pl2d", "T", "Tl1d", "Tl2d", "Ql1d", "Ql2d", "Q")     #   rename the data frame
  
  stn.z<-zoo(dat1[ ,2:10], as.Date(dat1[, 1], origin = "1970-01-01"))
  
  assign(stName, stn.z)
  
  #-------------   Nueral Network - NNET ------------------------------
  
  
  ## Split data into training and test data
  
  inTrain1 <- createDataPartition(y = dat1$P, p = .8, list = FALSE) ## the outcome data are needed, the percentage of data in the training set
  
  ## The format of the results
  
  training <- dat1[inTrain1,]
  testing <- dat1[ - inTrain1,]
  testQ <- testing$Q
  trainDates <- training$Dates
  testDates <- testing$Dates
  
  my.grid <- expand.grid(.decay = c(0, 0.01, 0.1, 10), .size = c(1, 5, 9))
  
  test.len <- dim(testing)[1]
  
  training <- training[, -1]
  
  #-------------   Nueral Network - NNET ------------------------------
  
  #nnet.model(training,testing)
  
  #-------------   Support Vector Machines - SVM ------------------------------
  
  #svm.model(training,testing)
  
  #-------------   Generalized Boosted Regression Models - GBM ----------------
  
  #gbm.model(training,testing)
  
 
  #------------- -----------  Model Trees  - M5 ----------------
  
  #m5n.model(training,testing)
  
  
  #-------------   Model Trees  - Cubist  ----------------
  
  #m5c.model(training,testing)

  
  rctrl <- trainControl(method = "cv", number = 3, returnResamp = "all")
  set.seed(1)
  #ptm <- proc.time()
  #m5 <- M5P(Q ~ ., data = training)
  
  M5n <- caret::train(Q ~ ., data = training, method = "M5", trControl = rctrl,
               preProc = c("center", "scale"))
  #proc.time() - ptm
  
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
  #zoo.m5 <- zoo(predicted.m5[, 2], as.Date(predicted.m5[, 1], origin = "1970-01-01"))
  modelsDirName <- "Models" # Name of directory where to keep trained model 
  resultDirName<-"Results"
  save(M5n, file = paste(file.path(paste(getwd(), resultDirName, sep = "/"), modelsDirName), "/", "m5n_", stName, ".RData", sep = ""))
}
 