boost_df=read.csv("creditcard.csv") 
col<- c(-1) 
boost_df<- boost_df[,col] 
set.seed(12345)
training_data <- sample(nrow(boost_df),0.7*nrow(boost_df)) 
boost_train <- boost_df[training_data,] 
boost_test<-boost_df[-training_data,]

library(ROSE)
##Under Sampling
boost_data_under=ovun.sample(Class ~ ., data = boost_train, method = "under", N = 684, seed = 1)$data
(table(boost_data_under$Class))
##Over Sampling
boost_data_over=ovun.sample(Class ~ ., data = boost_train, method="over", N=398044,seed = 1)$data
table(boost_data_over$Class)
##Both Sampling
boost_data_both=ovun.sample(Class ~ ., data = boost_train, method="both", p=0.5, N=1000,seed = 1)$data
table(boost_data_both$Class)
##Rose Sampling
boost_data_rose <- ROSE(Class ~ ., data = boost_train, seed = 12345)$data
table(boost_data_rose$Class)

##smote
library(DMwR)
boost_train$Class=as.factor(boost_train$Class)
boost_data_smote <- SMOTE(Class ~ ., data= boost_train, perc.over = 100, perc.under=200)
table(boost_data_smote$Class)


##boosting under sampling
library(gbm)
set.seed(1)
boost_under=gbm(Class~.,data=boost_data_under,distribution="gaussian",n.trees=5000,interaction.depth=4) 
summary(boost_under)
par(mfrow=c(1,2)) 
boost_pred_under=predict(boost_under,newdata=boost_test,n.trees=5000) 

##boosting over sampling
library(gbm)
set.seed(1)
boost_over=gbm(Class~.,data=data_over,distribution="gaussian",n.trees=5000,interaction.depth=4) 
summary(boost_over)
par(mfrow=c(1,2)) 
boost_pred=predict(boost_over,newdata=test,n.trees=5000)

##boosting both sampling
library(gbm)
set.seed(1)
boost_both=gbm(Class~.,data=boost_data_both,distribution="gaussian",n.trees=5000,interaction.depth=4) 
summary(boost_both)
par(mfrow=c(1,2)) 
boost_pred_both=predict(boost_both,newdata=boost_test,n.trees=5000)

##boosting smote
library(gbm)
set.seed(1)
boost_smote=gbm(Class~.,data=boost_data_smote,distribution="gaussian",n.trees=5000,interaction.depth=4) 
summary(boost_smote)
par(mfrow=c(1,2)) 
boost_pred_smote=predict(boost_smote,newdata=boost_test,n.trees=5000)


##boosting rose
boost_rose=gbm(Class~.,data=boost_data_rose,distribution="gaussian",n.trees=5000,interaction.depth=4) 
summary(boost_rose)
par(mfrow=c(1,2)) 
boost_pred_rose=predict(boost_rose,newdata=boost_test,n.trees=5000)

##boosting without sampling
boost_actual=gbm(Class~.,data=boost_train,distribution="gaussian",n.trees=5000,interaction.depth=4) 
summary(boost_actual)
par(mfrow=c(1,2)) 
boost_pred_actual=predict(boost_actual,newdata=boost_test,n.trees=5000)






##Confusion matrix Under sampling
x<-ifelse(boost_pred_under>0.5,1,0) 
boost_table_under<-table(boost_test$Class,x,dnn=list('actual','predicted'))
boost_table_under
##Confusion matrix over sampling
x<-ifelse(boost_pred_over>0.5,1,0)
boost_table_over<- table(boost_test$Class,x,dnn=list('actual','predicted'))
boost_table_over
##Confusion matrix both sampling
x<-ifelse(boost_pred_both>0.5,1,0)
boost_table_both<- table(boost_test$Class,x,dnn=list('actual','predicted'))
boost_table_both
##Confusion matrix smote sampling
x<-ifelse(boost_pred_smote>0.5,1,0)
boost_table_smote<- table(boost_test$Class,x,dnn=list('actual','predicted'))
boost_table_smote
##Confusion matrix rose
x<-ifelse(boost_pred_rose>0.5,1,0)
boost_table_rose<- table(boost_test$Class,x,dnn=list('actual','predicted'))
boost_table_rose
##Confusion matrix without sampling
x<-ifelse(boost_pred_actual>0.5,1,0)
boost_table_actual<- table(boost_test$Class,x,dnn=list('actual','predicted'))
boost_table_actual

##Accuracy under sampling
boost_acc_under=(boost_table_under[1]+boost_table_under[4])/(boost_table_under[2]+boost_table_under[4]+boost_table_under[3]+boost_table_under[1])
boost_acc_under





##Accuracy over sampling
boost_acc_over=(boost_table_over[1]+boost_table_over[4])/(boost_table_over[2]+boost_table_over[4]+boost_table_over[3]+boost_table_over[1])
boost_acc_over
##Accuracy both sampling
boost_acc_both=(boost_table_both[1]+boost_table_both[4])/(boost_table_both[2]+boost_table_both[4]+boost_table_both[3]+boost_table_both[1])
boost_acc_both
##Accuracy smote
boost_acc_smote=(boost_table_smote[1]+boost_table_smote[4])/(boost_table_smote[2]+boost_table_smote[4]+boost_table_smote[3]+boost_table_smote[1])
boost_acc_smote
##Accuracy rose
boost_acc_rose=(boost_table_rose[1]+boost_table_rose[4])/(boost_table_rose[2]+boost_table_rose[4]+boost_table_rose[3]+boost_table_rose[1])
boost_acc_rose
##Accuracy without sampling
boost_acc_actual=(boost_table_actual[1]+boost_table_actual[4])/(boost_table_actual[2]+boost_table_actual[4]+boost_table_actual[3]+boost_table_actual[1])
boost_acc_actual







##Specificity under sampling
boost_spe_under=(boost_table_under[1]/(boost_table_under[1]+boost_table_under[3]))
boost_spe_under
##Specificity over sampling
boost_spe_over=(boost_table_over[1]/(boost_table_over[1]+boost_table_over[3]))
boost_spe_over
##Specificity both sampling
boost_spe_both=(boost_table_both[1]/(boost_table_both[1]+boost_table_both[3]))
boost_spe_both
##Specificity smote
boost_spe_smote=(boost_table_smote[1]/(boost_table_smote[1]+boost_table_smote[3]))
boost_spe_smote

##Specificity without sampling
boost_spe_actual=(boost_table_actual[1]/(boost_table_actual[1]+boost_table_actual[3]))
boost_spe_actual
##Specificity rose
boost_spe_rose=(boost_table_rose[1]/(boost_table_rose[1]+boost_table_rose[3]))
boost_spe_rose








##ROCR under sampling
library(ROCR)
accuracy.meas(boost_test[,30], as.numeric(boost_pred_under))
boost_pred.under <- prediction( as.numeric(boost_pred_under), boost_test[,30] )
boost_perf_under <- performance( boost_pred.under, "tpr", "fpr" )
boost_perf1_under <- performance( boost_pred.under, measure = "auc" )
boost_perf2_under <- performance( boost_pred.under, "prec", "rec" )
boost_auc_under <- boost_perf1_under@y.values[[1]]
boost_auc_under


##ROCR over sampling
accuracy.meas(boost_test[,30], as.numeric(boost_pred_over))
boost_pred.over <- prediction( as.numeric(boost_pred_over), boost_test[,30] )
boost_perf_over <- performance( boost_pred.over, "tpr", "fpr" )
boost_perf1_over <- performance( boost_pred.over, measure = "auc" )
boost_perf2_over <- performance( boost_pred.over, "prec", "rec" )
boost_auc_over <- boost_perf1_over@y.values[[1]]
boost_auc_over


##ROCR both sampling
accuracy.meas(boost_test[,30], as.numeric(boost_pred_both))
boost_pred.both <- prediction( as.numeric(boost_pred_both), boost_test[,30] )
boost_perf_both <- performance( boost_pred.both, "tpr", "fpr" )
boost_perf1_both <- performance( boost_pred.both, measure = "auc" )
boost_perf2_both <- performance( boost_pred.both, "prec", "rec" )
boost_auc_both <- boost_perf1_both@y.values[[1]]
boost_auc_both


##ROCR smote
accuracy.meas(boost_test[,30], as.numeric(boost_pred_smote[,2]))
boost_pred.smote <- prediction( as.numeric(boost_pred_smote), boost_test[,30] )
boost_perf_smote <- performance( boost_pred.smote, "tpr", "fpr" )
boost_perf1_smote <- performance( boost_pred.smote, measure = "auc" )
boost_perf2_smote<- performance( boost_pred.smote, "prec", "rec" )
boost_auc_smote <- boost_perf1_smote@y.values[[1]]
boost_auc_smote

##ROCR without sampling
accuracy.meas(boost_test[,30], as.numeric(boost_pred_actual))
boost_pred.actual <- prediction( as.numeric(boost_pred_actual), boost_test[,30] )
boost_perf_actual <- performance( boost_pred.actual, "tpr", "fpr" )
boost_perf1_actual <- performance( boost_pred.actual, measure = "auc" )
boost_perf2_actual <- performance( boost_pred.actual, "prec", "rec" )
boost_auc_actual <- boost_perf1_actual@y.values[[1]]
boost_auc_actual

##ROCR rose
accuracy.meas(boost_test[,30], as.numeric(boost_pred_rose))
boost_pred.rose <- prediction( as.numeric(boost_pred_rose), boost_test[,30] )
boost_perf_rose <- performance( boost_pred.rose, "tpr", "fpr" )
boost_perf1_rose <- performance( boost_pred.rose, measure = "auc" )
boost_perf2_rose <- performance( boost_pred.rose, "prec", "rec" )
boost_auc_rose <- boost_perf1_rose@y.values[[1]]
boost_auc_rose





##ROC curve
plot( boost_perf_actual , main="ROC curve", col = "black")
abline(a = 0, b = 1, lty = 2,col="red")
plot( boost_perf_over, col = "green", add = TRUE)
plot( boost_perf_both , col = "yellow", add = TRUE)
plot( boost_perf_smote , col = "pink", add = TRUE)
plot( boost_perf_under , col = "black", add = TRUE)
plot( boost_perf_rose , col = "purple", add = TRUE)

##Precision-Recall curve
plot(boost_perf2_actual , main="Precision-Recall curve", col = "blue")
abline(a = 0, b = 1, lty = 2,col="red")
plot( boost_perf2_over, col = "green", add = TRUE)
plot( boost_perf2_both , col = "yellow", add = TRUE)
plot( boost_perf2_smote , col = "pink", add = TRUE)
plot( boost_perf2_under , col = "black", add = TRUE)
plot( boost_perf2_rose , col = "purple", add = TRUE)




