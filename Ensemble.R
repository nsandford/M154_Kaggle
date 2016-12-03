library(dplyr)
library(e1071)
library(randomForest)
library(gbm)
library(xgboost)
Train <- read.csv("training_imputed.csv")
Test <- read.csv("test_imputed.csv")
Train$X <- NULL
Test$X <- NULL
Test$SeriousDlqin2yrs <- NULL

GBMModel <- function(dfTrain) {
  GB <- gbm(dfTrain$SeriousDlqin2yrs ~ ., data=dfTrain, n.trees=5000,
            keep.data=FALSE, shrinkage=0.01, bag.fraction=0.3,
            interaction.depth=10)
  GB
}

forestModel <- function(dfTrain) {
  train.Forest <- randomForest(formula = as.factor(SeriousDlqin2yrs) ~ ., 
                               data = Train, ntree = 2000, importance = TRUE, 
                               na.action=na.omit)
  train.Forest
}

svmModel <- function(dfTrain) {
  train.svm <- svm(as.factor(SeriousDlqin2yrs) ~ ., 
                   type='C-classification', kernel='radial', 
                   cachesize=2000, probability=TRUE, cost=1, 
                   data = dfTrain)
  train.svm
}

xgbModel <- function(dfTrain, dfTest) {
  test_names <- names(dfTest)
  train_names <- names(dfTrain)
  intersect_names <- intersect(test_names, train_names)
  
  training <- xgb.DMatrix(data = data.matrix(dfTrain[intersect_names]),label=(dfTrain[,c("SeriousDlqin2yrs")]))
  
  # subsample - setting it to 0.5 means that XGBoost randomly collected half of the data instances to grow 
  # trees and this will prevent overfitting. 
  # colsample_bytree - subsample ratio of columns when constructing each tree.
  param <- list(objective = "binary:logistic", booster = "gbtree", eta = 0.01, max_depth = 50,
                subsample = 0.5, colsample_bytree = 0.5)
  
  train.xgb <- xgboost(data = training, params = param, nrounds = 500, verbose = 1, eval_metric="auc")
  
  train.xgb
}

ensemble <- function(dfTrain, dfTest) {
  #SVM commented out because does not improve AUC score
  models <- list(rfs=forestModel(dfTrain),
                           gbs=GBMModel(dfTrain),
                           #svm=svmModel(dfTrain),
                           xgb=xgbModel(dfTrain, dfTest))
  models
}

ensemblePredictions <- function(model, train, test) {
  pred <- predict(model$rfs, test, type = 'prob')[,2]
  pred2 <- predict(model$gbs, test, n.trees = 1000)
  #pred3 <- predict(model$svm, test)
  
  test_names <- names(test)
  train_names <- names(train)
  intersect_names <- intersect(test_names, train_names)

  testDataXGB <- xgb.DMatrix(data = data.matrix(test[,intersect_names]))
  pred4 <- predict(xgb1,testDataXGB)
  
  pred <- data.frame(Id = seq.int(nrow(test)), ProbabilityRF=pred, 
                     ProbabilityGBM=pred2, ProbabilityXGB=pred4)
  pred$ProbabilityGBM <- 1/(1+exp(-pred$ProbabilityGBM))
  
  # After submitting each model individually, this upweights models with greater individual AUC
  pred$Probability <- ((pred$ProbabilityGBM)*4 + pred$ProbabilityRF + (pred$ProbabilityXGB)*20)/25
  
  pred <- pred %>% dplyr::select(Id, Probability)
  write.csv(pred, file="probabilities4-1-10.csv", row.names=FALSE)
}

ensemble.models <- ensemble(Train, Test)
ensemblePredictions(ensemble.models, Train, Test)
