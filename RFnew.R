if (!require(randomForest)) install.packages("randomForest")

library(randomForest)

set.seed(123)
# 建立分类随机森林模型
train_data$sepsis3  <- as.factor(train_data$sepsis3)
Rf.biop <- randomForest(sepsis3~firstday_InvasiveVent + admission_age +paraplegia  + firstday_sofa + firstday_apsiii 
                        + firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean 
                        + resp_rate_min + temperature_max +temperature_mean+ glucose_max+firstday_urineoutput + wbc_max 
                        + calcium_min +chloride_max +glucose_lab_max , data = train_data, type = "classification")

Rf.biop
plot(Rf.biop)

which.min(rf.biop$err.rate[, 1])
#[1] 41

set.seed(123)
rf.biop.2 <- randomForest(sepsis3 ~ firstday_InvasiveVent + admission_age +paraplegia  + firstday_sofa + firstday_apsiii 
                          + firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean 
                          + resp_rate_min + temperature_max +temperature_mean+ glucose_max+firstday_urineoutput + wbc_max 
                          + calcium_min +chloride_max +glucose_lab_max , data = train_data, ntree = 247)
getTree(rf.biop, 1)

rf.biop.2


rf.biop.test <- predict(rf.biop.2, newdata = test_data)
test_data$rf.biop.<- predict(rf.biop.2, newdata = test_data, type = "prob")
table(rf.biop.test, train_data$sepsis3)

test_data$sui  <- as.factor(train_data$sui)
confusionMatrix(rf.biop.test, test_data$sepsis3, positive = "1")
varImpPlot(rf.biop.2)

ROC<-roc(train_data$sepsis3,rf.biop.test)
auc(ROC)
ci(auc(ROC))

#复杂的ROC曲线以及具体变量值

plot(ROC, print.auc = TRUE, print.thres = TRUE, auc.polygon = TRUE, auc.polygon.col = "lightblue", auc.polygon.alpha = 0.3, 
     main = "ROC Curve(Test)", 
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")
ROC.results<-coords(ROC,"best",ret="all",transpose=FALSE)
as.matrix(ROC.results)

title("ROC Curve(Train)")
title(xlab = "1 - Specificity")
title(ylabel="Sensitivity")
threshold <- 0.236
specificity <- 1
sensitivity <- 1
accuracy <- 1
text(0.3, 0.3, paste("Threshold: ", round(threshold, 4)), pos = 4)
text(0.3, 0.25, paste("Specificity: ", round(specificity, 4)), pos = 4)
text(0.3, 0.2, paste("Sensitivity: ", round(sensitivity, 4)), pos = 4)
text(0.3, 0.15, paste("Accuracy: ", round(accuracy, 4)), pos = 4)
roc.curve(rf.biop.test, train_data$sepsis3, main = "ROC curve of randomForest",
          col = 2, lwd = 2, lty = 2)
## Area under the curve (AUC): 0.969
legend("bottomright", "AUC:0.939", col = 2, lty = 1, lwd = 2, bty = "n")
train_data$sui <- factor(train_data$sui, levels = c(0,1), labels = c("No","Yes"))
test_data$sui <- factor(test_data$sui, levels = c(0,1), labels = c("No","Yes"))
write.csv(test_data, "/Users/maofengkai/Desktop/testsjsl.csv")
