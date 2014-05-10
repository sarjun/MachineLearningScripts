library(randomForest)
library(pROC)
library(DMwR)
library(plyr)

# Determine which methods/formula to use
formula<-Class ~ BP_D + BP_M + BP_S + Density_Score + SD_HR
useSMOTE<-F
useKnnImpute<-F
omitRowsWithMissingVals<-F

# Read in the data and set up train/test sets
folds = 10

data<-read.table("../DataSet2.csv",header=T,sep=",")

cvFolds = rmCrossValidation(data, folds)

# Apply preprocessing methods

# Build the logistic regression model and calculate ROC curve
rocAvg = 0
sensitivityList = list()
specificityList = list()
colors = rainbow(folds)
areas = list()

for(i in 1:folds) {
  testset = cvFolds[[i]][[2]]
  attach(cvFolds[[i]][[1]])
  optimal = randomForest(formula, na.action = na.exclude, ntree=25)
  #optimal = robcov(optimal,cluster=id)
  prob=predict(optimal,type=c("response"),newdata = testset)
  testset$prob = prob
  ROC = roc(Class==1 ~ prob, data = testset, auc=TRUE)
  rocAvg = rocAvg + as.numeric(ROC["auc"])
  areas[i] = as.numeric(ROC["auc"])
  sensitivityList[[i]] = ROC["sensitivities"]
  specificityList[[i]] = ROC["specificities"]
  if(i==1) {
    plot(ROC, col=colors[i])
  }
  if(i>1) {
    plot(ROC, add=TRUE, col=colors[i])
  }
}
areas = unlist(areas)
rocAvg = rocAvg / folds
print(rocAvg)
