library(rms)
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

# Build the logistic regression model and calculate ROC curve
optimal<-lrm(Class~HR+BP_S+BP_M+BP_D+LDS,y=T,x=T)
optimal<-robcov(optimal,cluster=id)
print(optimal)
prob=predict(optimal,type=c("lp"),testset)
testset$prob = prob
ROC <- roc(Class==1 ~ prob, data = testset)
plot(ROC)
