
#### NOTES: 
#1. Use same data preparation steps as Ishan.
#2. Use over, under, both, rose, and smote sampling techniques on training data.
#3. Use k-fold cross validation on each model and sampling combination.
#4. Determine AUC, Recall, Precision, F, and confusion matrix on every combination of model/sampling/kfold



library(caret)
library(e1071)
library(rpart)
library(randomForest)
library(ggplot2)
library(rattle)
library(tree)
library(RColorBrewer)
library(ROSE)
library(DMwR)
library(ROCR)


Credit_Fraud_Data <- read.csv("creditcard.csv")

any(is.na(Credit_Fraud_Data))
summary(Credit_Fraud_Data)

## We can see there is a class imbalance.
table(Credit_Fraud_Data$Class)
prop.table(table(Credit_Fraud_Data$Class))

#Removing unnecessary variables
Credit_Fraud_Data_1 <- Credit_Fraud_Data[,-1]
Credit_Fraud_Data_1$Class <- as.factor(Credit_Fraud_Data_1$Class)

###Partitionong Dataset
set.seed(12345)
train <- sample(nrow(Credit_Fraud_Data_1),0.7*nrow(Credit_Fraud_Data_1))
df_train <-Credit_Fraud_Data_1[train,]
dim(df_train)
df_test <- Credit_Fraud_Data_1[-train,]
dim(df_test)


##################################
##   Sampling Techniques Used ####
##################################

###Over Sampling
df_train_over <- ovun.sample(Class ~ ., data = df_train, method = "over",N = 398044)$data
table(df_train_over$Class)

###Under Sampling
df_train_under <- ovun.sample(Class ~ ., data = df_train, method = "under",N = 684, seed = 12345)$data
table(df_train_under$Class)


###Both Over and Under Sampling
df_train_both <- ovun.sample(Class ~ ., data = df_train, method = "both", p=0.5, N = 100000, seed =12345)$data
table(df_train_both$Class)

###ROSE
df_train_rose <- ROSE(Class ~ ., data = df_train, seed = 12345)$data
table(df_train_rose$Class)

###SMOTE
df_train_smote <- SMOTE(Class ~ ., data= df_train, perc.over = 100, perc.under=200)
table(df_train_smote$Class)



## Simple Logistic Regression Classification based on 70/30 data segmentation.
FitLog <- glm(Class~., family = "binomial", data = df_train)
summary(FitLog)


#Predict
p.test <- predict(FitLog, df_test, type="response")
summary(p.test)

class.pred.log <- ifelse(p.test > 0.5, 1, 0)
table(df_test$Class, class.pred.log)
as.matrix(table(df_test$Class, class.pred.log))[1,1]/sum(as.matrix(table(df_test$Class, class.pred.log))[1,])


#AIC: 1632.5
accuracy.meas(df_test$Class, p.test)

#cutoff: 0.5
#precision: 0.889
#recall: 0.599
#F: 0.358

roc.curve(df_test$Class, p.test, plotit = F)
#Area under the curve (AUC): 0.944

#Logistic Regression on balanced data
log.rose <- glm(Class~., data = df_train_rose, family = "binomial")

log.over <- glm(Class~., data = df_train_over, family = "binomial")

log.under <- glm(Class~., data = df_train_under, family = "binomial")

log.both <- glm(Class~., data = df_train_both, family = "binomial")

log.smote <- glm(Class~., data = df_train_smote, family = "binomial")

#preditions on unseen data
pred.log.rose <- predict(log.rose, newdata = df_test, type="response")
pred.log.over <- predict(log.over, newdata = df_test, type="response")
pred.log.under <- predict(log.under, newdata = df_test, type="response")
pred.log.both <- predict(log.both, newdata = df_test, type="response")
pred.log.smote <- predict(log.smote, newdata = df_test, type="response")

#MEASURES USED TO COMPARE SAMPLING TECHNIQUES.
#Precision: It is a measure of correctness achieved in positive prediction i.e. of observations labeled as positive, how many are actually labeled positive.
#Precision = TP / (TP + FP)

#Recall: It is a measure of actual observations which are labeled (predicted) correctly i.e. how many observations of positive class are labeled correctly. It is also known as 'Sensitivity'.
#Recall = TP / (TP + FN)

#F measure: It combines precision and recall as a measure of effectiveness of classification in terms of ratio of weighted importance on either recall or precision as determined by ?? coefficient.
#F measure = ((1 + ??)² × Recall × Precision) / ( ??² × Recall + Precision )
#?? is usually taken as 1.


#AUC Rose
roc.curve(df_test$Class, pred.log.rose, plotit = F)
#AUC: 0.986
accuracy.meas(df_test$Class, pred.log.rose)
#cutoff: 0.5
#precision: 0.129
#recall: 0.871
#F: 0.112
class.log.rose <- ifelse(pred.log.rose > 0.5, 1, 0)
table(df_test$Class, class.log.rose)
as.matrix(table(df_test$Class, class.log.rose))[1,1]/sum(as.matrix(table(df_test$Class, class.log.rose))[1,])

#AUC Over-Sampling
roc.curve(df_test$Class, pred.log.over)
#AUC: 0.986
accuracy.meas(df_test$Class, pred.log.over, plotit = F)
#cutoff: 0.5
#precision: 0.150
#recall: 0.898
#F: 0.129
class.log.over <- ifelse(pred.log.over > 0.5, 1, 0)
table(df_test$Class, class.log.over)
as.matrix(table(df_test$Class, class.log.over))[1,1]/sum(as.matrix(table(df_test$Class, class.log.over))[1,])

#AUC Under-Sampling
roc.curve(df_test$Class, pred.log.under, plotit = F)
#AUC: 0.978
accuracy.meas(df_test$Class, pred.log.under)
#cutoff: 0.5
#precision: 0.013
#recall: 0.959
#F: 0.013
class.log.under <- ifelse(pred.log.under > 0.5, 1, 0)
table(df_test$Class, class.log.under)
as.matrix(table(df_test$Class, class.log.under))[1,1]/sum(as.matrix(table(df_test$Class, class.log.under))[1,])

#AUC Both
roc.curve(df_test$Class, pred.log.both, plotit = F)
#AUC: 0.985
accuracy.meas(df_test$Class, pred.log.both)
#cutoff: 0.5
#precision: 0.068
#recall: 0.918
#F: 0.063
class.log.both <- ifelse(pred.log.both > 0.5, 1, 0)
table(df_test$Class, class.log.both)
as.matrix(table(df_test$Class, class.log.both))[1,1]/sum(as.matrix(table(df_test$Class, class.log.both))[1,])


#AUC SMOTE
roc.curve(df_test$Class, pred.log.smote, col = 'red', plotit = F)

#AUC: 0.970
accuracy.meas(df_test$Class, pred.log.smote)
#precision: 0.038
#recall: 0.900
#F: 0.037

class.log.smote <- ifelse(pred.log.smote > 0.5, 1, 0)
table(df_test$Class, class.log.smote)
as.matrix(table(df_test$Class, class.log.smote))[1,1]/sum(as.matrix(table(df_test$Class, class.log.smote))[1,])


###Plotting ROC curves from each graph
roc.curve(df_test$Class, p.test, col='blue')
roc.curve(df_test$Class, pred.log.smote, col = 'pink', add.roc = TRUE)
roc.curve(df_test$Class, pred.log.both, col = 'yellow', add.roc = TRUE)
roc.curve(df_test$Class, pred.log.under, col = 'black', add.roc = TRUE)
roc.curve(df_test$Class, pred.log.rose, col = 'purple', add.roc = TRUE)
roc.curve(df_test$Class, pred.log.over, col = 'green', add.roc = TRUE)
