library(neuralnet)
intub<-read.table("anontfmodel2_R_grouped.csv",header=T,fill =T,sep=",")
training <- intub[1:246509,]
testing <- intub[246510:372527,]
m <- model.matrix(~ Class + age + SD_RESP + SPO2_R + Density_Score + HR + LDS + SD_SPO2_perc, data = training)
# attach(training)
x <- testing[, c( "age", "SD_RESP", "SPO2_R", "Density_Score", "HR", "LDS", "SD_SPO2_perc")]
full<-neuralnet(formula = Class ~ age + SD_RESP + SPO2_R + Density_Score + HR + LDS + SD_SPO2_perc, data = m, hidden = 8, err.fct="sse", rep=1)

plot(full)

# attach(testing)
prob=compute(full, x)
testing$prob = prob$net.result
library(pROC)
ROC <- roc(Class==1 ~ prob, data = testing)
plot(ROC)
ls()