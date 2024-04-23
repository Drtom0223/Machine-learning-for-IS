library(class)
library(caret)
library(sampling)
library(ggplot2)
library(tidyverse)
library(kknn)
library(crosstable)
library(gmodels)

k = ceiling(sqrt(1567))  # 参数取24是因为训练的样本有288个，开根后是48
k
## [1] 40


knn.pred <- knn(train = train_data[, c("firstday_InvasiveVent", "admission_age", "paraplegia", "firstday_sofa", "firstday_apsiii",
                                       "firstday_lods", "gcs_verbal1", "heart_rate_min", "heart_rate_mean",
                                       "resp_rate_min", "temperature_max", "temperature_mean", "glucose_max", "firstday_urineoutput", "wbc_max",
                                       "calcium_min", "chloride_max", "glucose_lab_max"
)], 
                test = test_data[, c("firstday_InvasiveVent", "admission_age", "paraplegia", "firstday_sofa", "firstday_apsiii",
                                     "firstday_lods", "gcs_verbal1", "heart_rate_min", "heart_rate_mean",
                                     "resp_rate_min", "temperature_max", "temperature_mean", "glucose_max", "firstday_urineoutput", "wbc_max",
                                     "calcium_min", "chloride_max", "glucose_lab_max"
                )], 
                cl = train_data$sepsis3, 
                k = k)


# 进行k折交叉验证
control <- trainControl(method = 'cv',number = 10)
grid1 <- expand.grid(.k = seq(2, 40, by = 1))
# knn模型训练
train_data$sepsis3 <- as.factor(train_data$sepsis3)
knn.pred <- train(sepsis3 ~ firstday_InvasiveVent + admission_age + paraplegia + firstday_sofa + firstday_apsiii +
                  firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean +
                  resp_rate_min + temperature_max + temperature_mean + glucose_max + firstday_urineoutput + wbc_max +
                  calcium_min + chloride_max + glucose_lab_max,
                method = 'knn',
                preProcess = c('center','scale'),
                trControl = control,
                tuneGrid = grid1,
                data = train_data,
                tuneLength = 10)
knn.pred

table(train_data$sui)



ctrl <- trainControl(method = "cv", number = 10)
knn.pred <- train(
  x = train_data[, c("firstday_InvasiveVent", "admission_age", "paraplegia", "firstday_sofa", "firstday_apsiii", 
                     "firstday_lods", "gcs_verbal1", "heart_rate_min", "heart_rate_mean", "resp_rate_min", "temperature_max", 
                     "temperature_mean", "glucose_max", "firstday_urineoutput", "wbc_max", "calcium_min", "chloride_max", "glucose_lab_max")],
  y = train_data$sepsis3,
  method = "knn",
  tuneLength = 10,
  trControl = ctrl
)
knn.pred

plot(knn.pred$results$k, knn.pred$results$RMSE, type = "l", xlab = "K", ylab = "RMSE",
     lwd = 2)
points(knn.pred$results$k, knn.pred$results$RMSE, col = "red", pch = 20, cex = 2)
abline(v = 23, col = "grey", lwd = 1.5)



# 查看预测结果
head(test_pred)
###########################
###########################
# 加载需要的库
library(caret)
library(pROC)

# 首先,我们需要提取出测试集的真实标签和预测概率
true_labels <- test_data$sepsis3

# 然后,使用pROC包计算ROC曲线
ROC<-roc(train_data$sepsis3,test_pred)
auc(ROC)
ci(auc(ROC))

#复杂的ROC曲线以及具体变量值

# 绘制 ROC 曲线
plot(ROC, print.auc = TRUE, print.thres = TRUE, auc.polygon = TRUE, auc.polygon.col = "lightblue", auc.polygon.alpha = 0.3, 
     main = "ROC Curve(Train)", 
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")
ROC.results<-coords(ROC,"best",ret="all",transpose=FALSE)
as.matrix(ROC.results)

title("ROC Curve(Train)")
title(xlab = "1 - Specificity")
title(ylabel="Sensitivity")
threshold <- 0.236
specificity <- 0.709
sensitivity <- 0.740
accuracy <- 0.722
text(0.3, 0.3, paste("Threshold: ", round(threshold, 4)), pos = 4)
text(0.3, 0.25, paste("Specificity: ", round(specificity, 4)), pos = 4)
text(0.3, 0.2, paste("Sensitivity: ", round(sensitivity, 4)), pos = 4)
text(0.3, 0.15, paste("Accuracy: ", round(accuracy, 4)), pos = 4)

