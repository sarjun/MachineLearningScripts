library(rms)
library(pROC)
library(DMwR)
library(plyr)

# Determine which methods/formula to use
formula<-Class ~ age+ HR + SPO2_perc+ SPO2_R+ SD_HR+ SD_SPO2_perc+ SD_SPO2_R+ HR_SPO2+ COSEn+ LDS+ Density_Score+ BP_S+ BP_D+ BP_M
useSMOTE<-F
useKnnImpute<-F
omitRowsWithMissingVals<-F

# Read in the data and set up train/test sets
folds = 10
data = read.table("C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/DataSet2.csv",header=T,sep=",")
cvFolds = rmCrossValidation(data, folds)

# Apply preprocessing methods

# Build the logistic regression model and calculate ROC curve
rocAvg = 0
sensitivityList = list()
specificityList = list()
for(i in 1:folds) {
  testset = cvFolds[[i]][[2]]
  attach(cvFolds[[i]][[1]])
  optimal = lrm(formula,y=T,x=T)
  optimal = robcov(optimal,cluster=id)
  prob=predict(optimal,type=c("lp"),testset)
  testset$prob = prob
  ROC = roc(Class==1 ~ prob, data = testset, auc=TRUE)
  rocAvg = rocAvg + as.numeric(ROC["auc"])
  sensitivityList[[i]] = ROC["sensitivities"]
  specificityList[[i]] = ROC["specificities"]
  if(i==1) {
    plot(ROC)
  }
  if(i>1) {
    plot(ROC, add=TRUE)
  }
}

rocAvg = rocAvg / folds
print(rocAvg)
