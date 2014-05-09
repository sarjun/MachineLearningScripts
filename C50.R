library(pROC)
library(C50)
# data<-read.table("anontfmodel2_R_grouped.csv",header=T,fill =T,sep=",")
# training <- data[1:246509,]
# testing <- data[246510:372527,]
# attach(training)
folds = 10
data<-read.table("C:/Users/Arjun/Documents/UVa/Sixth Semester/CS 6316/ResearchProject/DataSet2.csv",header=T,sep=",")
cvFolds = rmCrossValidation(data, folds)

rocAvg = 0
colors = rainbow(folds)
for(i in 1:folds) {
  testing = cvFolds[[i]][[2]]
  training = cvFolds[[i]][[1]]
  attach(training)
  x <- training[, c("SD_SPO2_R", "SPO2_R", "HR", "BP_D", "BP_M", "BP_S", "Density_Score", "SD_HR")]
  y <- factor(training[,"Class"])
  full<-C5.0(x, y, rule=F, control = C5.0Control(CF = 0.25, winnow=F))

  testset <- testing[, c("SD_SPO2_R", "SPO2_R", "HR", "BP_D", "BP_M", "BP_S", "Density_Score", "SD_HR")]
  prob=predict.C5.0(full, newdata = testset, type=c("prob"), trials = 1)
  testing$probA = prob[,"0"]
  testing$probB = prob[,"1"]
  ROC <- roc(Class==1 ~ probB, data = testing)
  rocAvg = rocAvg + as.numeric(ROC["auc"])
  if(i==1) {
    plot(ROC, col=colors[i], main="C5.0 Decision Tree")
  }
  if(i>1) {
    plot(ROC, add=TRUE, col=colors[i])
  }
}

rocAvg = rocAvg / folds
print(rocAvg)