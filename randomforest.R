library(randomForest)
intub<-read.table("anontfmodel2_R_grouped.csv",header=T,fill =T,sep=",")
training <- intub[1:246509,]
testing <- intub[246510:372527,]
attach(training)
# vital<-lrm(y~HR+RR+SPO2+RR_SPO2,y=T,x=T)
# vital<-robcov(vital,cluster=ID)
# print(vital)
# library("gam")
full<-randomForest(formula = Class ~ age+ HR + SPO2_perc+ SPO2_R+ SD_HR+ SD_SPO2_perc+ SD_SPO2_R+ HR_SPO2+ COSEn+ LDS+ Density_Score+ BP_S+ BP_D+ BP_M, na.action = na.exclude, ntree=25)
# full<-robcov(full,cluster=id)
print(full)

# attach(testing)
prob=predict(full,type=c("response"),newdata = testing)
testing$prob = prob
library(pROC)
ROC <- roc(Class==1 ~ prob, data = testing)
plot(ROC)

# hrmodel<-lrm(y~HR,y=T,x=T)
# hrmodel<-robcov(hrmodel,cluster=ID)
# print(hrmodel)
