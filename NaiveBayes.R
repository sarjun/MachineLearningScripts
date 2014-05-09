library(e1071)
library(pROC)

# Determine which methods/formula to use
formula<-Class ~ age + HR + SPO2_R + Density_Score + LDS + BP_M + BP_D + SD_SPO2_R + SPO2_perc

# Read in the data and set up train/test sets
folds = 10
data<-read.table("C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/DataSet2.csv",header=T,sep=",")
cvFolds = rmCrossValidation(data, folds)

# Build the decision tree and calculate ROC curve
rocAvg = 0
colors = rainbow(folds)
for(i in 1:folds) {
  testset = cvFolds[[i]][[2]]
  trainset = cvFolds[[i]][[1]]
  attach(trainset)
  TrainData <- trainset[,1:(ncol(trainset)-1)]
  TrainClasses <- as.data.frame(trainset[,ncol(trainset)])
  names(TrainClasses) = c("id")
  bayes <- naiveBayes(TrainData, TrainClasses, formula=formula)
  prob=predict(bayes,type=c("raw"),testset)
  testset$prob = prob[,"1"]
  ROC <- roc(Class==1 ~ prob, data = testset)
  rocAvg = rocAvg + as.numeric(ROC["auc"])
  if(i==1) {
    plot(ROC, col=colors[i])
  }
  if(i>1) {
    plot(ROC, add=TRUE, col=colors[i])
  }
}
rocAvg = rocAvg / folds
print(rocAvg)
