setwd("~/Documents/Pomona/Pomona_School/2016_Fall/M154/kaggle/M154_Kaggle") # Nathan's data dir

Train <- read.csv('cs-training.csv', header=TRUE,comment.char="")
Train$X <- NULL # Removes redundant indexing
SeriousDlqin2yrs <- Train$SeriousDlqin2yrs
Train$SeriousDlqin2yrs <- NULL

N_var <- length(colnames(Train)) # Number of variables
N_obs <- length(Train$age) # Number of Observations
Test <- read.csv('cs-test.csv', header=TRUE, comment.char="")
Test$X <- NULL # Removes redundant indexing
Test$SeriousDlqin2yrs <- NULL # Removes redundant indexing

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
  print(paste("MI: ",df$MonthlyIncome[index[1]]),sep="") # Check for convergence
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
  print(paste("NoD: ",df$NumberOfDependents[index[1]]),sep="") # Check for convergence
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
  print(paste("DR: ",df$DebtRatio[index[1]]),sep="") # Check for convergence
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

Train <- Impute(Train)
Train$SeriousDlqin2yrs <- SeriousDlqin2yrs
Test <- Impute(Test)

write.csv(Train,"training_imputed.csv")
write.csv(Test,"test_imputed.csv")


