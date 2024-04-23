# 加载所需的库
install.packages("rsesel")
library(caret)
library(e1071)
library(tidyverse)
library(rfesel)


# 调优 SVM 模型的超参数
# 这里使用 caret 包中的 train 函数进行网格搜索
set.seed(456)
tuneGrid <- expand.grid(C = c(0.1, 1, 10, 100),
                        sigma = c(0.1, 1, 10))

# 将 sepsis3 转换为二分类的因子变量
train_data$sepsis3 <- as.factor(train_data$sepsis3)

svmModel <- train(sepsis3 ~ firstday_InvasiveVent + admission_age + paraplegia + firstday_sofa + firstday_apsiii + 
                    firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean + resp_rate_min + temperature_max + 
                    temperature_mean + glucose_max + firstday_urineoutput + wbc_max + calcium_min + chloride_max + 
                    glucose_lab_max,
                  data = train_data,
                  method = "svmRadial",
                  tuneGrid = tuneGrid,
                  trControl = trainControl(method = "cv", number = 10))
svmModel

# 在训练集上进行交叉验证
# 这里使用模型的 cv() 方法
# 使用训练好的 SVM 模型进行 10 折交叉验证
cvResults <- train(
  sepsis3 ~ firstday_InvasiveVent + admission_age + paraplegia + firstday_sofa + firstday_apsiii + 
    firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean + resp_rate_min + temperature_max + 
    temperature_mean + glucose_max + firstday_urineoutput + wbc_max + calcium_min + chloride_max + 
    glucose_lab_max,
  data = train_data,
  method = "svmRadial",
  tuneGrid = data.frame(C = svmModel$bestTune$C, 
                        sigma = svmModel$bestTune$sigma),
  trControl = trainControl(method = "cv", number = 10)
)
cvResults
# 使用 RFE 算法选择最优特征子集
rfeControl <- rfeControl(functions = rfFuncs, 
                         method = "cv", 
                         number = 10)
rfeResult <- rfe(train_data[, c("firstday_InvasiveVent", "admission_age", "paraplegia", "firstday_sofa", "firstday_apsiii",
                                "firstday_lods", "gcs_verbal1", "heart_rate_min", "heart_rate_mean", "resp_rate_min", "temperature_max",
                                 "temperature_mean", "glucose_max", "firstday_urineoutput", "wbc_max", "calcium_min", "chloride_max", "glucose_lab_max")], train_data$sepsis3, 
                 sizes = c(3, 5, 7, 9, 11), 
                 rfeControl = rfeControl)

# 基于选择的特征子集重新训练 SVM 模型
selectedFeatures <- names(rfeResult$optVariables)
selectedFeatures
var <- paste(rfeResult$optVariables, collapse = "+")
var
fun <- as.formula(paste("sepsis3", "~", var))
svm <- svm(fun, data = train_data, kernel = "radial")

# 在测试集上评估模型
predictions <- predict(svm, test_data[, rfeResult$optVariables])
accuracy <- sum(predictions == test_data$sepsis3) / nrow(test_data)
accuracy

predictions <- predict(svm, train_data[, rfeResult$optVariables], decision.values = TRUE)

library(pROC)
ROC<-roc(train_data$sepsis3,predictions)
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
specificity <- 0.835
sensitivity <- 0.858
accuracy <- 0.846
text(0.3, 0.3, paste("Threshold: ", round(threshold, 4)), pos = 4)
text(0.3, 0.25, paste("Specificity: ", round(specificity, 4)), pos = 4)
text(0.3, 0.2, paste("Sensitivity: ", round(sensitivity, 4)), pos = 4)
text(0.3, 0.15, paste("Accuracy: ", round(accuracy, 4)), pos = 4)

