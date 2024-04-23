install.packages(c("car","corrplot","leaps","glmnet","caret"))
install.packages("rempsyc")
library(car) #package to calculate Variance Inflation Factor（VIF）
library(corrplot) #correlation plots
library(leaps) #best subsets regression
library(glmnet) #allows ridge regression, LASSO and elastic net
library(caret) #this will help identify the appropriate parameters
library(epiDisplay)
library(foreign) 
library(rms)
library(caret)
library(dplyr)
library(pROC)
library(haven)
library(rempsyc)
library(broom)

dataDCA <- read.csv("/Users/maofengkai/Desktop/4月转运/final3.csv")

dataDCA[, 4:45] <- lapply(dataDCA[, 4:45], factor)

#进行数据标准化，离差标准化（Min-Max Scaling），将数据映射到 [0, 1] 的区间内
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

num_cols <- ncol(dataDCA)

# 创建一个空数据框来存储标准化后的结果

dataDCAb <- dataDCA %>%
  mutate(across(-c(3:45), normalize))
# 标准化除了第4-6列以外的所有列
for (i in 1:num_cols) {
  if (i %in% c(3:45)) {
    # 保留第4-6列不变
    dataDCAb[, i] <- dataDCA[, i]
  } else {
    # 标准化其他列
    dataDCAb[, i] <- normalize(dataDCA[, i])
  }
}

str(dataDCAb)

set.seed(2)
train_index <- createDataPartition(dataDCAb$sepsis3, p = 0.7, list = FALSE)
train_data <- dataDCAb[train_index, ]
test_data <- dataDCAb[-train_index, ]

# 再次检查训练集和测试集的结局变量 sepsis3 分布是否均衡
prop.table(table(train_data$sepsis3))

prop.table(table(test_data$sepsis3))
3.##LASSO回归
#建立训练与验证子集
#train.subfit<- subset(data20220905TlikeIRQ, group == 1) [,7:65]
#str(train.subfit202209905)
#test.subfit20220905 <- subset(data20220905TlikeIRQ, group == 2) [,7:65]
#str(test.subfit202209905)
#数据转变为矩阵
#train.subfit20220905<- cbind(train[,2:59],train.subfit20220905[,1])#把响应变量放到最后一排。
#names(train.subfit20220905)#把变量的名字列出来看看。
#test.subfit20220905<- cbind(test.subfit20220905[,2:59],test.subfit20220905[,1])#把响应变量放到最后一排。
#names(test.subfit20220905)#把变量的名字列出来看看。
set.seed(2)
x <- as.matrix(train_data[, -3])  
y <- as.matrix(train_data[, 3]) 

#x <- as.matrix(train[, 1:60])
#y <- train$hos_mort


lasso <- glmnet(x,y,family = "binomial", alpha = 1, nlambda = 1000)  #nlambda默认为100，哔哩哔哩视频都是1000
print(lasso)
View(print(lasso))
#画图如下
plot(lasso, xvar="lambda", label = TRUE)
#某具体lambda所对应的系数值
#lasso.coef <- predict(lasso, s = 0.03949, type = "coefficients")
#lasso.coef
#进一步在验证集验证
#newx <- as.matrix(test.subfit[, 1:60]) 
#lasso.y <-  predict(lasso, newx = newx, type = "response", s =0.03598)
#plot(lasso.y, test.subfit$hos_mort, xlab="Predicted" ,ylab="Actural",main="LASSO")
#计算残差平方均值MSE
#lasso.resid <- lasso.y - test.subfit20220905$hos_mort 
#mean(lasso.resid^2)
#0.1143


5.##交叉验证。 K折交叉验证。cv.glmnet()估计lambda之时默认使用10折交叉验证。
#在K折交叉验证中，数据被划分成K个相同的子集（折），每次使用K-1个子集拟合模型，然后使用剩下的那个子集做测试集
#，最后将K次拟合的结果综合起来（一般取平均值），确定最后的参数。在这个方法中，每个子集只有一次用作测试集。
#在glmnet包中使用K折交叉验证非常容易,结果每次拟和的lanbda值和响应的MSE.
#默认设置爱法等于1，所以如果你想试试领回归或弹性网络必须制定a值，因为我们想看看尽可能少的输入特征的情况
#所以还是使用默认设置。
set.seed(1314)
lasso.cv= cv.glmnet(x,y, nfolds = 10)
plot(lasso.cv)
#通过以下代码得到上图所示两条垂直虚线所对应的lambda值。
lasso.cv$lambda.min #minium
lasso.cv$lambda.1se  #最小值加一个标准误。
#使用lambda.1se可以完成下面过程。
coef(lasso.cv, s= "lambda.1se")

# 获取 lasso.cv 模型中 s= "lambda.1se" 的系数



#得到的17个特征是：
#
firstday_InvasiveVent + admission_age +paraplegia  + firstday_sofa + firstday_apsiii 
+ firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean 
+ resp_rate_min + temperature_max +temperature_mean+ glucose_max+firstday_urineoutput + wbc_max 
+ calcium_min +chloride_max +glucose_lab_max
formula1 <- as.formula(sepsis3 ~ firstday_InvasiveVent + admission_age + paraplegia + firstday_sofa + firstday_apsiii +
                        firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean +
                        resp_rate_min + temperature_max + temperature_mean + glucose_max + firstday_urineoutput + wbc_max +
                        calcium_min + chloride_max + glucose_lab_max)



fit1<-glm( formula1, data=train_data,family=binomial())
fit1
summary(fit1)


#fit2<- step(fit1,trace = FALSE)
#summary(fit2)
#glm建模（最终9个变量）


(formula <- as.formula(sepsis3 ~ firstday_sofa + firstday_apsiii  + paraplegia +admission_age +resp_rate_min+firstday_urineoutput))
fit2<-glm(formula, data=train_data,family=binomial())
summary(fit2)

table1<-logistic.display(fit2)
#计算OR及CI
exp(cbind("OR"= coef(fit2),confint(fit2))  )

#检验VIF
table2<-vif(fit2)
#nice_table(tidy(fit2),broom = lm)????
write.csv(table1,file = "/Users/maofengkai/Desktop/4月转运/结果/logistic/OR-CI-P.csv")
write.csv(table1,file = "/Users/maofengkai/Desktop/4月转运/结果/logistic/VIF.csv")
##lasso及logistic回归后确定最终变量做nomogram
#为rms包构建回归模型做准备
attach(train_data)#数据加载到当前工作环境
dd<- datadist(train_data)#打包数据
options(datadist='dd')   #打包数据

fit3<- lrm(formula, data=train_data ,x=T,y=T)
fit3
summary(fit3)

coef(fit3)
exp(coef(fit3))





library(rms)
dd= datadist(train_data)#打包数据
options(datadist='dd')

cal1 <- calibrate(fit3, cmethod='hare', method = 'boot', B = 1000) 
cal1 
plot(cal1,xlim = c(0,1),ylim = c(0,1),
     xlab="Predicted mortality",
     ylab="Actual mortality",
     plottitle = '(training mortality)') 

#输出c指数和改良C指数
v<- validate(fit3,method = 'boot',B=1000,dxy=T)
Dxy =v[rownames(v)=="Dxy",colnames(v)=="index.corrected"]
orig_Dxy = v[rownames(v)=="Dxy",colnames(v)=="index.orig"]
bias_corrected_c_index <- abs(Dxy)/2+0.5
orig_c_index <- abs(orig_Dxy )/2+0.5
c <- rcorrcens(sepsis3~predict(fit3),data = train_data)
lower <- c[1,1]-1.96*c[1,4]/2
upper <- c[1,1]+1.96*c[1,4]/2
Cindex<-rbind(orig_c_index,lower,upper,bias_corrected_c_index)
Cindex
#gfs实验，计算C指数
install.packages('ResourceSelection')
library(ResourceSelection)
model <- glm(formula,data = train_data,family = binomial(link = logit))
h1 <- hoslem.test(model$y,fitted(model),g=4)
h1
#############

pred.logit1 <-predict(fit3) #计算logit P，俗称的线性预测值
P1 <- 1/(1+exp(-pred.logit1)) #计算预测概率P
m=NROW(train_data)/3
model<-glm (formula,data = train_data,family = "binomial")#拟合模型
pred.logit2 <-predict(model)
P2<-predict(model,type = "response")

val.prob(P1,train_data$sepsis3,m=m,cex=0.8)
val.prob(P2,train_data$sepsis3,cex=0.8)
#计算brier评分以及校准曲线绘制


#开始对外部数据进行验证
fit4<-lrm (formula,data = test_data,x=TRUE,y=TRUE)
cal2<-calibrate(fit4,method="boot",B=1000)
plot(cal2,xlim = c(0,1),ylim = c(0,1),
     xlab="Predicted mortality",
     ylab="Actual mortality",
     plottitle = '(text mortality)') 
#输出c指数和改良C指数
v<- validate(fit4,method = 'boot',B=1000,dxy=T)
Dxy =v[rownames(v)=="Dxy",colnames(v)=="index.corrected"]
orig_Dxy = v[rownames(v)=="Dxy",colnames(v)=="index.orig"]
bias_corrected_c_index <- abs(Dxy)/2+0.5
orig_c_index <- abs(orig_Dxy )/2+0.5
c <- rcorrcens(sepsis3~predict(fit4),data = test_data)
lower <- c[1,1]-1.96*c[1,4]/2
upper <- c[1,1]+1.96*c[1,4]/2
Cindex<-rbind(orig_c_index,lower,upper,bias_corrected_c_index)
Cindex
###colnames(test)[45] <- 'Aniongap'

#
library(ResourceSelection)
model <- glm(formula,data = test_data,family = binomial(link = logit))
h1 <- hoslem.test(model$y,fitted(model),g=4)
h1

pred.logit1 <-predict(fit4) #计算logit P，俗称的线性预测值
P1 <- 1/(1+exp(-pred.logit1)) #计算预测概率P
m=NROW(test_data)/3
model<-glm (formula,data = test_data,family = "binomial")#拟合模型
pred.logit2 <-predict(model)
P2<-predict(model,type = "response")

val.prob(P1,test_data$sepsis3,m=m,cex=0.8)
val.prob(P2,test_data$sepsis3,cex=0.8)


###########################
#做简单的ROC曲线(train)
library(pROC)
model<-glm(formula,data=train_data,family = binomial())
train_data$predvalue <- predict(model,type = "response")

ROC<-roc(train_data$sepsis3,train_data$predvalue)
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


threshold <- 0.236
specificity <- 0.742
sensitivity <- 0.844
accuracy <- 0.787
text(0.3, 0.3, paste("Threshold: ", round(threshold, 4)), pos = 4)
text(0.3, 0.25, paste("Specificity: ", round(specificity, 4)), pos = 4)
text(0.3, 0.2, paste("Sensitivity: ", round(sensitivity, 4)), pos = 4)
text(0.3, 0.15, paste("Accuracy: ", round(accuracy, 4)), pos = 4)

#做简单的ROC曲线(test)

model<-glm(formula,data=test_data,family = binomial())
test_data$predvalue <- predict(model,type = "response")
library(pROC)
ROC<-roc(test_data$sepsis3,test_data$predvalue)
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
specificity <- 0.755
sensitivity <- 0.79
accuracy <- 0.770
text(0.3, 0.3, paste("Threshold: ", round(threshold, 4)), pos = 4)
text(0.3, 0.25, paste("Specificity: ", round(specificity, 4)), pos = 4)
text(0.3, 0.2, paste("Sensitivity: ", round(sensitivity, 4)), pos = 4)
text(0.3, 0.15, paste("Accuracy: ", round(accuracy, 4)), pos = 4)


#计算外部验证数据的概率
testDCA$pred.e <- predict(object = model.train, #指定模型
                         newdata = testDCA,   #指定数据集
                         type = "response")    #预测概率
library(rms)
#验证数据中绘制校准曲线
val.prob(testDCA$pred.e, testDCA$aki_stage)


####### dichotomous outcome
#DCA用rmda包做，但该包只能做二分类结局的logit回归。
#
install.packages("rmda")
library(rmda)
#
452/(1702)=0.2656
#建立一个简单的模型。thresholds是指设置X轴的一个概率范围。

#study.design = 'case-control',
#population.prevalence = 0.26
model1<- decision_curve(formula  ,data = train, 
                      family = binomial(link ='logit'),
                      thresholds= seq(0,1, by = 0.01),
                      confidence.intervals = 0.95,
                      study.design = 'cohort')
LODS<- decision_curve(survival90 ~  lods ,data = train, 
                           family = binomial(link ='logit'),
                           thresholds= seq(0,1, by = 0.01),
                           confidence.intervals = 0.95,
                           study.design = 'cohort'
                           )
GCS<- decision_curve(survival90 ~  first_gcs ,data = train, 
                      family = binomial(link ='logit'),
                      thresholds= seq(0,1, by = 0.01),
                      confidence.intervals = 0.95,
                     study.design = 'cohort')

OASIS<- decision_curve(survival90~  oasis ,data = train, 
                      family = binomial(link ='logit'),
                      thresholds= seq(0,1, by = 0.01),
                      confidence.intervals = 0.95,
                      study.design = 'cohort')
CHARLSON<- decision_curve(survival90~  charlson_comorbidity_index ,data = train,
                        family = binomial(link ='logit'),
                        thresholds= seq(0,1, by = 0.01),
                        confidence.intervals = 0.95,
                        study.design = 'cohort')
SOFA<- decision_curve(survival90 ~ min_sofa_icu ,data = train, 
                       family = binomial(link ='logit'),
                       thresholds= seq(0,1, by = 0.01),
                       confidence.intervals = 0.95,
                      study.design = 'cohort')
APSIII<- decision_curve(survival90 ~ apsiii ,data = train, 
                      family = binomial(link ='logit'),
                      thresholds= seq(0,1, by = 0.01),
                      confidence.intervals = 0.95,
                      study.design = 'cohort')



List<- list(model1,SOFA)#把几个模型组合起来。
List<- list(model1,GCS,SOFA,APSIII,LODS,CHARLSON,OASIS)#
#下面对组合好的两个模型画图。curve.name是出图时图例中曲线的名字，书写顺序要跟list时相同。
#col设置颜
plot_decision_curve(List, curve.names=c('Nomogram','SOFA'),
                    cost.benefit.axis = TRUE, col= c('red','blue'),
                    confidence.intervals= FALSE,
                    standardize = TRUE,
                    xlim = c(0,1))
 plot_decision_curve(List, curve.names=c('Nomogram','GCS','SOFA','APSIII','LODS','CHARLSON','OASIS'),
                    cost.benefit.axis = TRUE, col= c('red','yellow','green','black','purple','pink','blue'),
                    confidence.intervals= FALSE,
                    standardize = FALSE,
                    xlim = c(0,0.8))

##在test数据库继续验证DCA
#122/(122+629)=0.1624501

#建立一个简单的模型。thresholds是指设置X轴的一个概率范围。
testmodel1<- decision_curve(formula  ,data = test, 
                         family = binomial(link ='logit'),
                         thresholds= seq(0,1, by = 0.01),
                         confidence.intervals = 0.95,
                         study.design = 'cohort')
 testLODS<- decision_curve(survival90 ~  lods ,data = test, 
                       family = binomial(link ='logit'),
                       thresholds= seq(0,1, by = 0.01),
                       confidence.intervals = 0.95,
                       study.design = 'cohort'
 )
 testGCS<- decision_curve(survival90 ~  first_gcs ,data = test, 
                      family = binomial(link ='logit'),
                      thresholds= seq(0,1, by = 0.01),
                      confidence.intervals = 0.95,
                      study.design = 'cohort')
 
 testOASIS<- decision_curve(survival90~  oasis ,data = test, 
                        family = binomial(link ='logit'),
                        thresholds= seq(0,1, by = 0.01),
                        confidence.intervals = 0.95,
                        study.design = 'cohort')
 testCHARLSON<- decision_curve(survival90~  charlson_comorbidity_index ,data = test,
                           family = binomial(link ='logit'),
                           thresholds= seq(0,1, by = 0.01),
                           confidence.intervals = 0.95,
                           study.design = 'cohort')
 testSOFA<- decision_curve(survival90 ~ min_sofa_icu ,data = test, 
                       family = binomial(link ='logit'),
                       thresholds= seq(0,1, by = 0.01),
                       confidence.intervals = 0.95,
                       study.design = 'cohort')
 testAPSIII<- decision_curve(survival90 ~ apsiii ,data = test, 
                         family = binomial(link ='logit'),
                         thresholds= seq(0,1, by = 0.01),
                         confidence.intervals = 0.95,
                         study.design = 'cohort')
 
 
 
 List<- list(model1,SOFA)#把几个模型组合起来。
 List<- list(testmodel1,testGCS,testSOFA,testAPSIII,testLODS,testCHARLSON,testOASIS)#
 #下面对组合好的两个模型画图。curve.name是出图时图例中曲线的名字，书写顺序要跟list时相同。
 #col设置颜
 plot_decision_curve(List, curve.names=c('Nomogram','SOFA'),
                     cost.benefit.axis = TRUE, col= c('red','blue'),
                     confidence.intervals= FALSE,
                     standardize = TRUE,
                     xlim = c(0,1))
 plot_decision_curve(List, curve.names=c('Nomogram','GCS','SOFA','APSIII','LODS','CHARLSON','OASIS'),
                     cost.benefit.axis = TRUE, col= c('red','yellow','green','black','purple','pink','blue'),
                     confidence.intervals= FALSE,
                     standardize = FALSE,
                     xlim = c(0,0.8))
plot_decision_curve()




######################为C-INDEX进行数据准备
library(haven)
train <-  read_dta("E:/桌面/Cerebral Infarction预测模型-AKI/2-R语言数据处理/R-7比3-222-分组-trainDCA.dta")
#处理因子
#二分类是否处理为因子，不影响结果
#多分类变量必须处理为因子factor

train$marital_status_destring<- factor(train$marital_status_destring,levels =c(1,2,3,4,5),labels =c("Married","Single","Widowed","Divorced","Other"))
train$dementia<- factor(train$dementia,levels =c(0,1),labels =c("No","Yes"))
train$malignant_cancer<- factor(train$malignant_cancer,levels =c(0,1),labels =c("No","Yes"))
train$metastatic_solid_tumor<- factor(train$metastatic_solid_tumor,levels =c(0,1),labels =c("No","Yes"))
train$mannitol_use_firstday<- factor(train$mannitol_use_firstday,levels =c(0,1),labels =c("No","Yes"))
train$firstday_invasivevent<- factor(train$firstday_invasivevent,levels =c(0,1),labels =c("No","Yes"))

library(haven)
test <-  read_dta("E:/桌面/Cerebral Infarction预测模型-AKI/2-R语言数据处理/R-7比3-222-分组-testDCA.dta")
#处理因子
#二分类是否处理为因子，不影响结果
#多分类变量必须处理为因子factor

test$marital_status_destring<- factor(test$marital_status_destring,levels =c(1,2,3,4,5),labels =c("Married","Single","Widowed","Divorced","Other"))
test$dementia<- factor(test$dementia,levels =c(0,1),labels =c("No","Yes"))
test$malignant_cancer<- factor(test$malignant_cancer,levels =c(0,1),labels =c("No","Yes"))
test$metastatic_solid_tumor<- factor(test$metastatic_solid_tumor,levels =c(0,1),labels =c("No","Yes"))
test$mannitol_use_firstday<- factor(test$mannitol_use_firstday,levels =c(0,1),labels =c("No","Yes"))
test$firstday_invasivevent<- factor(test$firstday_invasivevent,levels =c(0,1),labels =c("No","Yes"))
###colnames(test)[45] <- 'Aniongap'

7.##C-INDEX  ROC Bilibili大鹏的方法
install.packages("pROC") #绘制ROC曲线
library(pROC) #绘制ROC曲线
#

fmCIMPM<-as.formula("aki_stage  ~ weight + congestive_heart_failure +heart_rate_mean +glucose_max_mmol  +firstday_urineoutput  +wbc_max +calcium_min+vasoactive_agent_firstday_use  +furosemide_use_firstday +firstday_invasivevent+  firstday_supplementaloxygen+  firstday_gcs_min")
fmGCS<-  as.formula("aki_stage  ~firstday_gcs_min")
fmSOFA<- as.formula("aki_stage  ~firstday_sofa")
fmAPSIII<-as.formula("aki_stage  ~firstday_apsiii")
fmLODS<- as.formula("aki_stage  ~firstday_lods")
fmSAPSII<-as.formula("aki_stage  ~firstday_sapsii")
fmOASIS<- as.formula("aki_stage  ~firstday_oasis")


modCIMPM<- glm(fmCIMPM, data = trainDCA, family = binomial())
modCIMPM
vif(modCIMPM)

modGCS<- glm(fmGCS, data = trainDCA, family = binomial())
modSOFA<- glm(fmSOFA, data = trainDCA, family = binomial())
modAPSIII<- glm(fmAPSIII, data = trainDCA, family = binomial())
modLODS<- glm(fmLODS, data = trainDCA, family = binomial())
modSAPSII<- glm(fmSAPSII, data = trainDCA, family = binomial())
modOASIS<- glm(fmOASIS, data = trainDCA, family = binomial())

#CIMPM
trainDCA$pre.CIMPM<- predict(modCIMPM,newdata = trainDCA, type =  "response")
ROC<-roc(trainDCA$aki_stage, trainDCA$pre.CIMPM)
auc(ROC)
ci(auc(ROC))
testDCA$pre.CIMPM<- predict(modCIMPM,newdata = testDCA, type =  "response")
ROC<-roc(testDCA$aki_stage, testDCA$pre.CIMPM)
auc(ROC)
ci(auc(ROC))

#gcs
trainDCA$pre.GCS<- predict(modGCS,newdata = trainDCA, type =  "response")
ROC<-roc(trainDCA$aki_stage, trainDCA$pre.GCS)
auc(ROC)
ci(auc(ROC))
testDCA$pre.GCS<- predict(modGCS,newdata = testDCA, type =  "response")
ROC<-roc(testDCA$aki_stage, testDCA$pre.GCS)
auc(ROC)
ci(auc(ROC))

#SOFA
trainDCA$pre.SOFA<- predict(modSOFA,newdata = trainDCA, type =  "response")
ROC<-roc(trainDCA$aki_stage, trainDCA$pre.SOFA)
auc(ROC)
ci(auc(ROC))
testDCA$pre.SOFA<- predict(modSOFA,newdata = testDCA, type =  "response")
ROC<-roc(testDCA$aki_stage, testDCA$pre.SOFA)
auc(ROC)
ci(auc(ROC))

#APSIII
trainDCA$pre.APSIII<- predict(modAPSIII,newdata = trainDCA, type =  "response")
ROC<-roc(trainDCA$aki_stage, trainDCA$pre.APSIII)
auc(ROC)
ci(auc(ROC))
testDCA$pre.APSIII<- predict(modAPSIII,newdata = testDCA, type =  "response")
ROC<-roc(testDCA$aki_stage, testDCA$pre.APSIII)
auc(ROC)
ci(auc(ROC))

#LODS
trainDCA$pre.LODS<- predict(modLODS,newdata = trainDCA, type =  "response")
ROC<-roc(trainDCA$aki_stage, trainDCA$pre.LODS)
auc(ROC)
ci(auc(ROC))
testDCA$pre.LODS<- predict(modLODS,newdata = testDCA, type =  "response")
ROC<-roc(testDCA$aki_stage, testDCA$pre.LODS)
auc(ROC)
ci(auc(ROC))

#SAPSII
trainDCA$pre.SAPSII<- predict(modSAPSII,newdata = trainDCA, type =  "response")
ROC<-roc(trainDCA$aki_stage, trainDCA$pre.SAPSII)
auc(ROC)
ci(auc(ROC))
testDCA$pre.SAPSII<- predict(modSAPSII,newdata = testDCA, type =  "response")
ROC<-roc(testDCA$aki_stage, testDCA$pre.SAPSII)
auc(ROC)
ci(auc(ROC))

#OASIS
trainDCA$pre.OASIS<- predict(modOASIS,newdata = trainDCA, type =  "response")
ROC<-roc(trainDCA$aki_stage, trainDCA$pre.OASIS)
auc(ROC)
ci(auc(ROC))
testDCA$pre.OASIS<- predict(modOASIS,newdata = testDCA, type =  "response")
ROC<-roc(testDCA$aki_stage, testDCA$pre.OASIS)
auc(ROC)
ci(auc(ROC))








####进行10折交叉验证
australian <- train_data
#将australian数据分成随机十等分
#install.packages("caret")
#固定folds函数的分组
set.seed(7)
require(caret)
folds <- createFolds(y=australian$sepsis3,k=10)

#构建for循环，得10次交叉验证的测试集精确度、训练集精确度

max=0
num=0

for(i in 1:10){
  
  fold_test <- australian[folds[[i]],] #取folds[[i]]作为测试集
  fold_train <- australian[-folds[[i]],] # 剩下的数据作为训练集
  
  print("***组号***")
  
  fold_pre <- glm(sepsis3 ~firstday_InvasiveVent + admission_age +paraplegia  + firstday_sofa + firstday_apsiii 
                  + firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean 
                  + resp_rate_min + temperature_max +temperature_mean+ glucose_max+firstday_urineoutput + wbc_max 
                  + calcium_min +chloride_max +glucose_lab_max,family=binomial(link='logit'),data=fold_train)
  fold_predict <- predict(fold_pre,type='response',newdata=fold_test)
  fold_predict =ifelse(fold_predict>0.5,1,0)
  fold_test$predict = fold_predict
  fold_error = fold_test[,113]-fold_test[,3]
  fold_accuracy = (nrow(fold_test)-sum(abs(fold_error)))/nrow(fold_test)
  print(i)
  print("***测试集精确度***")
  print(fold_accuracy)
  print("***训练集精确度***")
  fold_predict2 <- predict(fold_pre,type='response',newdata=fold_train)
  fold_predict2 =ifelse(fold_predict2>0.5,1,0)
  fold_train$predict = fold_predict2
  fold_error2 = fold_train[,113]-fold_train[,3]
  fold_accuracy2 = (nrow(fold_train)-sum(abs(fold_error2)))/nrow(fold_train)
  print(fold_accuracy2)
  
  
  if(fold_accuracy>max)
  {
    max=fold_accuracy
    num=i
  }
  
}

print(max)
print(num)

#十折里测试集最大精确度的结果
testi <- australian[folds[[8]],]
traini <- australian[-folds[[8]],]   # 剩下的folds作为训练集
prei <- glm(sepsis3 ~firstday_InvasiveVent + admission_age +paraplegia  + firstday_sofa + firstday_apsiii 
            + firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean 
            + resp_rate_min + temperature_max +temperature_mean+ glucose_max+firstday_urineoutput + wbc_max 
            + calcium_min +chloride_max +glucose_lab_max,family=binomial(link='logit'),data=traini)
predicti <- predict.glm(prei,type='response',newdata=testi)
predicti =ifelse(predicti>0.5,1,0)
testi$predict = predicti
#write.csv(testi,"ausfold_test.csv")
errori = testi[,113]-testi[,3]
accuracyi = (nrow(testi)-sum(abs(errori)))/nrow(testi) 

#十折里训练集的精确度
predicti2 <- predict.glm(prei,type='response',newdata=traini)
predicti2 =ifelse(predicti2>0.5,1,0)
traini$predict = predicti2
errori2 = traini[,113]-traini[,3]
accuracyi2 = (nrow(traini)-sum(abs(errori2)))/nrow(traini) 

#测试集精确度、取第i组、训练集精确
accuracyi;num;accuracyi2
#write.csv(traini,"ausfold_train.csv")

##
max = 0
num = 0

roc_obj_list = list()

for(i in 1:10){
  
  fold_test <- australian[folds[[i]],] #取folds[[i]]作为测试集
  fold_train <- australian[-folds[[i]],] # 剩下的数据作为训练集
  
  print("***组号***")
  
  fold_pre <- glm(sepsis3 ~firstday_InvasiveVent + admission_age +paraplegia  + firstday_sofa + firstday_apsiii 
                  + firstday_lods + gcs_verbal1 + heart_rate_min + heart_rate_mean 
                  + resp_rate_min + temperature_max +temperature_mean+ glucose_max+firstday_urineoutput + wbc_max 
                  + calcium_min +chloride_max +glucose_lab_max,family=binomial(link='logit'),data=fold_train)
  fold_predict <- predict(fold_pre,type='response',newdata=fold_test)
  fold_predict =ifelse(fold_predict>0.5,1,0)
  fold_test$predict = fold_predict
  fold_error = fold_test[,113]-fold_test[,3]
  fold_accuracy = (nrow(fold_test)-sum(abs(fold_error)))/nrow(fold_test)
  print(i)
  print("***测试集精确度***")
  print(fold_accuracy)
  print("***训练集精确度***")
  fold_predict2 <- predict(fold_pre,type='response',newdata=fold_train)
  fold_predict2 =ifelse(fold_predict2>0.5,1,0)
  fold_train$predict = fold_predict2
  fold_error2 = fold_train[,113]-fold_train[,3]
  fold_accuracy2 = (nrow(fold_train)-sum(abs(fold_error2)))/nrow(fold_train)
  print(fold_accuracy2)
  
  # 绘制ROC曲线
  roc_obj = roc(fold_test$sepsis3, fold_predict)
  roc_obj_list[[i]] = roc_obj
  
  if(fold_accuracy>max)
  {
    max=fold_accuracy
    num=i
  }
  
}

avg_roc = roc.hands(roc_obj_list)
pdf("roc_curve.pdf")
plot.roc(roc_obj_list[[1]], main="ROC Curves")
for (i in 2:10) {
  lines.roc(roc_obj_list[[i]], col=i)
}
legend("bottomright", paste0("Fold ", 1:10), col=1:10, lty=1)
dev.off()
print(max)
print(num)


best_roc_obj = roc_obj_list[[num]]
ci = ci.auc(best_roc_obj)
ci
best_roc_obj = roc_obj_list[[8]]
coords = coords(best_roc_obj, "best", ret=c("specificity", "sensitivity"))
spec = coords["specificity"]
sens = coords["sensitivity"]
spec
sens


###










