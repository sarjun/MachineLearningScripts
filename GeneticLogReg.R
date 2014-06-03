library(rms)
library(pROC)
library(DMwR)
library(plyr)

# Constants
phenotypesLoc = "C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/GA/phenotypes.txt"
archiveDir = "C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/GA/Archive/"
dataLoc = "C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/DataSet2.csv"

# Read in the data and set up train/test sets
folds = 10
data<-read.table(dataLoc,header=T,sep=",")
cvFolds = rmCrossValidation(data, folds)

# Set up cross validation function
# source('./CrossValidationFunction.R')

# Helpers for cross validation
attributeNames = colnames(data)[2:32]

readPhenotypes = function() {
  return(scan(phenotypesLoc, what="", sep="\n"))
}

attrsOfPhenotype = function(phenotype) {
  toReturn = list()
  for(i in 1:31) {
    if(substr(phenotype, i, i) == "1") {
      toReturn[length(toReturn) + 1] = attributeNames[i]
    }
  }
  
  return(toReturn)
}

evaluatePhenotype = function(phenotype) {
  # Get formula from phenotype
  formula = as.formula(paste("y~", paste(attrsOfPhenotype(phenotype), collapse="+")))
  
  # Build the logistic regression model and calculate ROC curve
  rocAvg = 0
  for(i in 1:folds) {
    testset = cvFolds[[i]][[2]]
    attach(cvFolds[[i]][[1]])
    optimal = lrm(formula,y=T,x=T)
    optimal = robcov(optimal,cluster=id)
    prob=predict(optimal,type=c("prob"),testset)
    testset$prob = prob
    ROC = roc(Class==1 ~ prob, data = testset, auc=TRUE)
    rocAvg = rocAvg + as.numeric(ROC["auc"])
  }
  
  rocAvg = rocAvg / folds
  print(rocAvg)
  
}