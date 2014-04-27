library(rms)
library(pROC)
library(DMwR)
library(plyr)

# Determine which methods/formula to use
formula<-Class ~ age+ HR + SPO2_perc+ SPO2_R+ SD_HR+ SD_SPO2_perc+ SD_SPO2_R+ HR_SPO2+ COSEn+ LDS+ Density_Score+ BP_S+ BP_D+ BP_M
useSMOTE<-T
useKnnImpute<-F
omitRowsWithMissingVals<-T

# Read in the data and set up train/test sets
data<-read.table("C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/DataSet2.csv",header=T,sep=",")
data<-data[with(data, order(id)), ]
splitIndex <- trunc(nrow(data)*0.66)
while(data$id[splitIndex]==data$id[splitIndex+1]) {
  splitIndex <- splitIndex + 1
}
trainset <- data[1:splitIndex,]
testset <- data[(splitIndex+1):nrow(data),]

# Apply preprocessing methods
if(omitRowsWithMissingVals) {
  trainset = na.omit(trainset)
}
if(useKnnImpute) {
  trainset <- knnImputation(trainset, k=5)
}
if(useSMOTE) {
  countRare <- count(trainset, c("Class"))[,c("freq")][2]
  countCommon <- count(trainset, c("Class"))[,c("freq")][1]
  Over <- ( (0.6 * countCommon) - countRare ) / countRare
  Under = (0.4 * countCommon) / (countRare * Over)
  Over_Perc = round(Over, 1) * 100
  Under_Perc = round(Under, 1) * 100
  
  trainset$Class <- as.factor(trainset$Class)
  trainset <- SMOTE(Class ~ ., trainset, perc.over = Over_Perc ,perc.under = Under_Perc)
}
attach(trainset)

# Build the logistic regression model and calculate ROC curve
optimal<-lrm(formula,y=T,x=T)
optimal<-robcov(optimal,cluster=id)
print(optimal)
prob=predict(optimal,type=c("lp"),testset)
testset$prob = prob
ROC <- roc(Class==1 ~ prob, data = testset)
plot(ROC)
