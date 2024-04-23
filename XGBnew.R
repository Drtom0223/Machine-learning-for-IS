if (!require(xgboost)) install.packages("xgboost")

if (!require(caret)) install.packages("caret")

if (!require(InformationValue)) install.packages("InformationValue")

library(InformationValue)
library(xgboost)
library(caret)
library(shapviz)

## 
train_data$sepsis3  <- as.factor(train_data$sepsis3)


# 选择参数范围
grid <- expand.grid(nrounds = c(40,50,60,70,80,90,100),
                    colsample_bytree = 1,
                    min_child_weight = 1,
                    eta = c(0.01, 0.05, 0.3),
                    gamma = c(0.5, 0.25),
                    subsample = 0.5,
                    max_depth = c(2, 3))

# 一些控制参数
cntrl <- trainControl(method = "cv",
                      number = 10,
                      verboseIter = F,
                      returnData = F,
                      returnResamp = "final")

# 开始调优
set.seed(1)
train.xgb <- train(x = train_data[ ,c("firstday_InvasiveVent", "admission_age", "paraplegia", "firstday_sofa", "firstday_apsiii", 
                                      "firstday_lods", "gcs_verbal1", "heart_rate_min", "heart_rate_mean", "resp_rate_min", "temperature_max", 
                                      "temperature_mean", "glucose_max", "firstday_urineoutput", "wbc_max", "calcium_min", "chloride_max", "glucose_lab_max")],
                   y = train_data$sepsis3,
                   trControl = cntrl,
                   tuneGrid = grid,
                   method = "xgbTree")

train.xgb
plot(train.xgb)



param <- list(objective = "binary:logistic", booster = "gbtree", eval_metric = "error",
                           eta = 0.05, max_depth =3, subsample = 0.5, colsample_bytree = 1, gamma = 0.25)

x <- as.matrix(train_data[ ,c("firstday_InvasiveVent", "admission_age", "paraplegia", "firstday_sofa", "firstday_apsiii", 
                              "firstday_lods", "gcs_verbal1", "heart_rate_min", "heart_rate_mean", "resp_rate_min", "temperature_max", 
                              "temperature_mean", "glucose_max", "firstday_urineoutput", "wbc_max", "calcium_min", "chloride_max", "glucose_lab_max")])
y <- ifelse(train_data$sepsis3 == "1", 0, 1)
train.mat <- xgb.DMatrix(data = x, label = y)

set.seed(123)
xgb.fit <- xgb.train(params = param, data = train.mat, nrounds = 100)
xgb.fit

impMatrix <- xgb.importance(feature_names = dimnames(x)[[2]], model = xgb.fit)
impMatrix
xgb.plot.importance(impMatrix, main = "Gain by Feature")

pred <- predict(xgb.fit, x)
optimalCutoff(y, pred)

roc_obj <- roc(train_data$sepsis3, pred)
plot(roc_obj, main="ROC Curve")
auc(roc_obj)
ci(auc(ROC))


testMat <- as.matrix(test_data[ ,c("firstday_InvasiveVent", "admission_age", "paraplegia", "firstday_sofa", "firstday_apsiii", 
                                   "firstday_lods", "gcs_verbal1", "heart_rate_min", "heart_rate_mean", "resp_rate_min", "temperature_max", 
                                   "temperature_mean", "glucose_max", "firstday_urineoutput", "wbc_max", "calcium_min", "chloride_max", "glucose_lab_max")])
xgb.test <- predict(xgb.fit, testMat)
y.test <- ifelse(test_data$sepsis3 == "1", 0, 1)

trainMat <- as.matrix(train_data[ ,c("firstday_InvasiveVent", "admission_age", "paraplegia", "firstday_sofa", "firstday_apsiii", 
                                   "firstday_lods", "gcs_verbal1", "heart_rate_min", "heart_rate_mean", "resp_rate_min", "temperature_max", 
                                   "temperature_mean", "glucose_max", "firstday_urineoutput", "wbc_max", "calcium_min", "chloride_max", "glucose_lab_max")])
xgb.train <- predict(xgb.fit, trainMat)
y.train <- ifelse(train_data$sepsis3 == "1", 0, 1)
optimalCutoff(y.train, xgb.train)
roc_obj <- roc(train_data$sepsis3, xgb.train)
plot(roc_obj, print.auc = TRUE, print.thres = TRUE, auc.polygon = TRUE, auc.polygon.col = "lightblue", auc.polygon.alpha = 0.3, 
     main = "ROC Curve(Train)", 
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")
ROC.results<-coords(roc_obj,"best",ret="all",transpose=FALSE)
as.matrix(ROC.results)
auc(roc_obj)
ci(auc(roc_obj))

specificity <- 0.848
sensitivity <- 0.848
accuracy <- 0.848

text(0.3, 0.25, paste("Specificity: ", round(specificity, 4)), pos = 4)
text(0.3, 0.2, paste("Sensitivity: ", round(sensitivity, 4)), pos = 4)
text(0.3, 0.15, paste("Accuracy: ", round(accuracy, 4)), pos = 4)



optimalCutoff(y.test, xgb.test)
roc_obj <- roc(test_data$sepsis3, xgb.test)
plot(roc_obj, print.auc = TRUE, print.thres = TRUE, auc.polygon = TRUE, auc.polygon.col = "lightblue", auc.polygon.alpha = 0.3, 
     main = "ROC Curve(Test)", 
     xlab = "1 - Specificity", 
     ylab = "Sensitivity")
ROC.results<-coords(ROC,"best",ret="all",transpose=FALSE)
as.matrix(ROC.results)
auc(roc_obj)
ci(auc(ROC))

specificity <- 0.755
sensitivity <- 0.79
accuracy <- 0.77

text(0.3, 0.25, paste("Specificity: ", round(specificity, 4)), pos = 4)
text(0.3, 0.2, paste("Sensitivity: ", round(sensitivity, 4)), pos = 4)
text(0.3, 0.15, paste("Accuracy: ", round(accuracy, 4)), pos = 4)


confusionMatrix(y.test, xgb.test, threshold = 0.29)

1 - misClassError(y.test, xgb.test, threshold = 0.29)

########################



##SHAP

shap_xgboost <- shapviz(xgb.fit, X_pred = as.matrix(test_data[ ,c("firstday_InvasiveVent", "admission_age", "paraplegia", "firstday_sofa", "firstday_apsiii", 
                                                          "firstday_lods", "gcs_verbal1", "heart_rate_min", "heart_rate_mean", "resp_rate_min", "temperature_max", 
                                                          "temperature_mean", "glucose_max", "firstday_urineoutput", "wbc_max", "calcium_min", "chloride_max", "glucose_lab_max")]))
#单个样本力图
sv_force(shap_xgboost,row_id = 2)

#去掉图片灰色背景
sv_importance(shap_xgboost,kind = "beeswarm")+theme_bw()
#变量重要性柱状图
sv_importance(shap_xgboost)+theme_bw()

sv_dependence(shap_xgboost, "firstday_sofa", 
              alpha = 0.5,
              size = 1.5,
              color_var = NULL)+theme_bw()
#多个变量偏相关依赖图
sv_dependence(shap_xgboost, 
              v = c("firstday_InvasiveVent",
                    "firstday_apsiii",
                    "admission_age",
                    "firstday_lods"))+theme_bw()

