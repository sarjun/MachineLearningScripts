library(e1071)
library(pROC)

# Read in the data and set up train/test sets
data<-read.table("C:/Users/Arjun/Dropbox/Smash Music/anontfmodel2_R.csv",header=T,sep=",")
data<-data[with(data, order(id)), ]
splitIndex <- trunc(nrow(data)*0.66)
while(data$id[splitIndex]==data$id[splitIndex+1]) {
  splitIndex <- splitIndex + 1
}
trainset <- data[1:splitIndex,]
testset <- data[(splitIndex+1):nrow(data),]
attach(trainset)

# Build the decision tree and calculate ROC curve
TrainData <- trainset[,1:(ncol(trainset)-1)]
TrainClasses <- trainset[,ncol(trainset)]
bayes <- naiveBayes(TrainData, TrainClasses, na.action=na.omit)
prob=predict(bayes,type=c("raw"),testset)
testset$prob = prob[,"1"]
ROC <- roc(Class==1 ~ prob, data = testset)
plot(ROC)
