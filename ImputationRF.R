library(randomForest)
library(fANCOVA)
Train <- read.csv('cs-training.csv', header=TRUE,comment.char="")
Train$X <- NULL # Removes redundant indexing

set.seed(890522)
o<-order(runif(dim(Train)[1]))
credit.train <- Train[o[1:(.9*(dim(Train)[1]))],]
credit.pred <- Train[o[(.9*(dim(Train)[1])+1):dim(Train)[1]],]

N_var <- length(colnames(Train)) # Number of variables
N_obs <- length(Train$age) # Number of Observations
Test <- read.csv('cs-test.csv', header=TRUE, comment.char="")
Test$X <- NULL # Removes redundant indexing

Update_MI <- function(df,index) {
  # Updates 0 and NA values of Monthly Income
  #
  # Args:
  #   df: data frame of observations
  #
  # Returns:
  #   dataframe w/ updated values
  LinReg <- lm(MonthlyIncome ~ .,df) # Performs linear regression for Monthly Income using all other variales
  df$MonthlyIncome[index] <- predict(LinReg,df[index,]) # Imputes for Monthly Income
  #print(paste("MI: ",df$MonthlyIncome[zero.na.MI.index[1]]),sep="") # Check for convergence
  return(df)
}

Update_NoD <- function(df,index) {
  # Updates NA values of Number of Dependents
  #
  # Args:
  #   df: data frame of observations
  #
  # Returns:
  #   dataframe w/ updated values
  LinReg <- lm(NumberOfDependents ~ .,df) # Performs linear regression for Monthly Income using all other variales
  df$NumberOfDependents[index] <- predict(LinReg,df[index,]) # Imputes for Number of Dependents
  #print(paste("NoD: ",df$NumberOfDependents[na.NoD.index[1]]),sep="") # Check for convergence
  return(df)
}

Update_DR <- function(df,index) {
  # Updates Debt Ratio for Monthly Income = 0 or NA
  #
  # Args:
  #   df: data frame of observations
  #
  # Returns:
  #   dataframe w/ updated values
  LinReg <- lm(DebtRatio ~ .,df) # Performs linear regression for Debt Ratio using all other variales
  df$DebtRatio[index] <- predict(LinReg,df[index,]) # Imputes for Debt Ratio
  #print(paste("DR: ",df$DebtRatio[zero.na.MI.index[1]]),sep="") # Check for convergence
  return(df)
}


Impute <- function(df) {
  # Imputes NA values in the data frame using linear regression
  #
  # Args:
  #   df: data frame of observations
  #
  # Returns:
  #   data frame with imputed values for Monthly Income, Debt Ratio, and Number of Dependents
  na.NoD.index <- which(is.na(df$NumberOfDependents)) # Observations with Number of Dependents = NA
  na.MI.index <- which(is.na(df$MonthlyIncome)) # Observations with Monthly Income = NA
  zero.MI.index <- which(df$MonthlyIncome == 0) # Observations with Monthly Income = 0
  zero.na.MI.index <- sort(c(na.MI.index,zero.MI.index)) # Observations with Monthly Income = 0 or NA
  
  Debt_Hold <- df$DebtRatio[zero.na.MI.index] # Stores Debt Values where Debt Ratio could not be calculated
  df$DebtRatio[zero.na.MI.index] <- NA # Replaces Debt Values with NA
  
  mean_NoD <- mean(df$NumberOfDependents,na.rm=TRUE) # Mean Number of Dependents
  mean_DR <- mean(df$DebtRatio,na.rm=TRUE) # Mean Debt Ratio
  mean_MI <- mean(df$MonthlyIncome,na.rm=TRUE) # Mean Monthly Income
  
  df$DebtRatio[zero.na.MI.index] <- mean_DR # Insert Mean Number of Dependents as init for Observations with Monthly Income = 0 or NA
  df$NumberOfDependents[na.NoD.index] <- mean_NoD # Insert Mean Number of Dependents as init for Observations with NoD = NA
  
  # Imputation
  for(i in 1:20) {
    df <- Update_MI(df,zero.na.MI.index)
    df <- Update_NoD(df,na.NoD.index)
    df <- Update_DR(df,zero.na.MI.index)
  }
  
  df$DebtRatio[zero.na.MI.index] <- Debt_Hold / df$MonthlyIncome[zero.na.MI.index] # Replace DR with original debt divided by imputed MI
  df$MonthlyIncome[zero.MI.index] <- 0 # Reset MI for those with original MI = 0 
  df$NumberOfDependents[df$NumberOfDependents < 0] <- 0 # Set NoD = 0 if imputed NoD < 0
  
  return(df)
}

Train <- Impute(credit.train)
Pred <- Impute(credit.pred)

train.Forest <- randomForest(formula = as.factor(SeriousDlqin2yrs) ~ ., data = Train, ntree = 500, importance = TRUE, na.action=na.omit)

evalModel <- function(rfModel, dfTrain, dfPred, k = 0.5) {
  # Check the accuracy of the model on the train dataset, and then test it on the held-out 10% of the data credit.pred
  # k ----> the bound on the probability we are willing to accept the model to predict a delinquint, assumed to be 0.5
  #      |k| <= 1
  credit.is.rf.predict<-as.vector(predict(rfModel, type="prob", newdata=dfTrain)[,2])
  credit.oos.rf.predict<-as.vector(predict(rfModel, type="prob", newdata=dfPred)[,2])
  
  # in-sample percent delinquints correctly predicted
  delinq.logit<-which(credit.is.rf.predict > k)
  accTrain <- sum(dfTrain$SeriousDlqin2yrs[delinq.logit])/length(delinq.logit)
  
  # out-of-sample percent survivors correctly predicted
  delinq.logit<-which(credit.oos.rf.predict > k)
  accPred <- sum(dfPred$SeriousDlqin2yrs[delinq.logit])/length(delinq.logit)
  
  # predict empirical probability with the Random Forest predicted probability
  loess.data<-as.data.frame(cbind(credit.oos.rf.predict, SeriousDlqin2yrs=dfPred$SeriousDlqin2yrs))
  credit.lo.rf.mod<-loess.as(loess.data$credit.oos.rf.predict, loess.data$SeriousDlqin2yrs, degree=1)
  credit.lo.rf.pred<-na.omit(predict(credit.lo.rf.mod, newdata=loess.data$credit.oos.rf.predict))
  
  oo<-order(na.omit(credit.oos.rf.predict)) # order the observations
  # plot empirical predicted probability against model predicted probability
  # i.e. take the actual proportion of delinquints and plot them against the predicted proportion of delinquints
  # a 45deg line would be a perfect representation
  # ------------------------------------------------------------------------
  plot(na.omit(credit.lo.rf.pred)[oo]~na.omit(credit.oos.rf.predict)[oo], type="l", ylim=c(0,1), xlab="random forest predicted Pr(delinq)", ylab="empirical probability Pr(delinq)")
  abline(0, 1, lty=2)
  rug(credit.oos.rf.predict)
  
  list(accTrain = accTrain, accPred = accPred)
}

evalModel(train.Forest, Train, Pred)


