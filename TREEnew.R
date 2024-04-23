if (!require(rpart)) install.packages("rpart")
if (!require(partykit)) install.packages("partykit")
if (!require(caret)) install.packages("caret")
if (!require(rpart.plot)) install.packages("rpart.plot")
install.packages("ROSE")
library(rpart)  #classification and regression trees
library(partykit)  #treeplots
library(caret)  #tune hyper-parameters
library(rpart.plot)

set.seed(123)
tree.pros <- rpart(sepsis3 ~ firstday_InvasiveVent + admission_age +paraplegia  + firstday_sofa + firstday_apsiii 
                   + firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean 
                   + resp_rate_min + temperature_max +temperature_mean+ glucose_max+firstday_urineoutput + wbc_max 
                   + calcium_min +chloride_max +glucose_lab_max, data = train_data, method = "class")
printcp(tree.pros)

rpart.plot(tree.pros)

tree.pros$cptable

plotcp(tree.pros)
cp <- min(tree.pros$cptable[3, ])
cp
prune.tree.pros <- prune(tree.pros, cp = cp)
plot(as.party(tree.pros))


##########
ctrl <- trainControl(method = "cv", number = 10)


train_data$sepsis3 <- as.factor(train_data$sepsis3)
bagged_cv <- train(sepsis3 ~ firstday_InvasiveVent + admission_age +paraplegia  + firstday_sofa + firstday_apsiii 
                   + firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean 
                   + resp_rate_min + temperature_max +temperature_mean+ glucose_max+firstday_urineoutput + wbc_max 
                   + calcium_min +chloride_max +glucose_lab_max, data = train_data, method = "treebag", trControl = ctrl)
bagged_cv

plot(varImp(bagged_cv),10)
############################
party.pros.test<- predict(prune.tree.pros, newdata = test_data,"prob")

party.pros.train<- predict(prune.tree.pros, newdata = train_data)

table(party.pros.train, train_data$sui)
test_data$sui  <- as.factor(test_data$sui)
confusionMatrix(party.pros.test, test_data$sui, positive = "1")

library(pROC)
ROC<-roc(traintree$train_pred[,"1"], train_data$sepsis3)
auc(ROC)
ci(auc(ROC))
plot(ROC, print.auc = TRUE, print.thres = FALSE, auc.polygon = TRUE, auc.polygon.col = "lightblue", auc.polygon.alpha = 0.3)
ROC.results<-coords(ROC,"best",ret="all",transpose=FALSE)
as.matrix(ROC.results)
title("ROC Curve(test)")
title(xlab = "1 - Specificity")
title(ylabel="Sensitivity")
threshold <- 0.236
specificity <- 0.915
sensitivity <- 0.658
accuracy <- 0.871
text(0.3, 0.3, paste("Threshold: ", round(threshold, 4)), pos = 4)
text(0.3, 0.25, paste("Specificity: ", round(specificity, 4)), pos = 4)
text(0.3, 0.2, paste("Sensitivity: ", round(sensitivity, 4)), pos = 4)
text(0.3, 0.15, paste("Accuracy: ", round(accuracy, 4)), pos = 4)

library(ROSE)
roc.curve(party.pros.test, test_data$sui, main = "ROC curve of Party", col = 2,
          lwd = 2, lty = 2)

