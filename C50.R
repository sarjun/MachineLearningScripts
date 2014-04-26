library(C50)
intub<-read.table("anontfmodel2_R_grouped.csv",header=T,fill =T,sep=",")
training <- intub[1:246509,]
testing <- intub[246510:372527,]
attach(training)

x <- training[, c("age", "HR", "SPO2_perc", "SPO2_R", "SD_HR", "SD_SPO2_perc", "SD_SPO2_R", "HR_SPO2", "COSEn", "LDS", "Density_Score", "BP_S", "BP_D", "BP_M")]
y <- factor(training[,"Class"])
full<-C5.0(x, y, rule=F, control = C5.0Control(CF = 0.25, winnow=F))

print(full)
testset <- testing[, c("age", "HR", "SPO2_perc", "SPO2_R", "SD_HR", "SD_SPO2_perc", "SD_SPO2_R", "HR_SPO2", "COSEn", "LDS", "Density_Score", "BP_S", "BP_D", "BP_M")]
# attach(testing)
prob=predict.C5.0(full, newdata = testset, type=c("prob"), trials = 1)
testing$probA = prob[,"0"]
testing$probB = prob[,"1"]
library(pROC)
ROC <- roc(Class==1 ~ probA, data = testing)
plot(ROC)
