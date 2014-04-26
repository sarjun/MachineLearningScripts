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
optimal<-lrm(Class ~ age+ HR + SPO2_perc+ SPO2_R+ SD_HR+ SD_SPO2_perc+ SD_SPO2_R+ HR_SPO2+ COSEn+ LDS+ Density_Score+ BP_S+ BP_D+ BP_M,
          y=T,x=T)
optimal<-robcov(optimal,cluster=id)
print(optimal)
prob=predict(optimal,type=c("lp"),testset)
testset$prob = prob
ROC <- roc(Class==1 ~ prob, data = testset)
plot(ROC)
