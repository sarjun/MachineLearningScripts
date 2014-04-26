library(rms)
library(pROC)
library(DMwR)

# Determine which methods/formula to use
useSMOTE<-T

# Read in the data and set up train/test sets
data<-read.table("C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/DataSet2.csv",header=T,sep=",")
data<-data[with(data, order(id)), ]
splitIndex <- trunc(nrow(data)*0.66)
while(data$id[splitIndex]==data$id[splitIndex+1]) {
  splitIndex <- splitIndex + 1
}
trainset <- data[1:splitIndex,]
testset <- data[(splitIndex+1):nrow(data),]
attach(trainset)

# Apply preprocessing methods
if(useSMOTE) {
  trainset <- 
}


# Build the logistic regression model and calculate ROC curve
optimal<-lrm(Class~HR+BP_S+BP_M+BP_D+LDS,y=T,x=T)
optimal<-robcov(optimal,cluster=id)
print(optimal)
prob=predict(optimal,type=c("lp"),testset)
testset$prob = prob
ROC <- roc(Class==1 ~ prob, data = testset)
plot(ROC)
