knn_df=read.csv("creditcard.csv")
col<- c(-1) 
knn_df_class <- knn_df$Class

knn_df<- knn_df[,col] 
scale(knn_df)
set.seed(12345)
training_data <- sample(nrow(knn_df),0.7*nrow(knn_df)) 
knn_train <- knn_df[training_data,] 
knn_test<-knn_df[-training_data,]

library(ROSE)
##under sampling
knn_data_under=ovun.sample(Class ~ ., data = knn_train, method = "under", N = 684, seed = 1)$data
(table(knn_data_under$Class))
##over sampling
knn_data_over=ovun.sample(Class ~ ., data = knn_train, method="over", N=398044,seed = 1)$data
table(knn_data_over$Class)
##both sampling
knn_data_both=ovun.sample(Class ~ ., data = knn_train, method="both", p=0.5, N=1000,seed = 1)$data
table(knn_data_both$Class)
##rose sampling
knn_data_rose <- ROSE(Class ~ ., data = knn_train, seed = 12345)$data
table(knn_data_rose$Class)
##smote sampling
library(DMwR)
knn_train$Class=as.factor(knn_train$Class)
knn_data_smote <- SMOTE(Class ~ ., data= knn_train, perc.over = 100, perc.under=200)
table(knn_data_smote$Class)

##knn under sampling




##knn without sampling

library(class)
x<-NULL
y<-NULL
for(i in 1:20){
  x <- knn(knn_data_under[,-30], knn_test[,-30], knn_data_under$Class, k=i)
  y[i] <- mean(knn_test$Class != x)
}
mink=which.min(y)

knn_under=knn(knn_data_under, knn_test, knn_data_under$Class, k=mink, prob=TRUE)

##knn over sampling
x<-NULL
y<-NULL
for(i in 1:20){
   x <- knn(knn_data_over[,-30], knn_test[,-30], knn_data_over$Class, k=i)
    y[i] <- mean(knn_test$Class != x)
 }
 
 
mink=which.min(y)

knn_over=knn(knn_data_over[,-30], knn_test[,-30], knn_data_over$Class, k=mink, prob = TRUE)

##knn both sampling
x<-NULL
y<-NULL
mink=NULL
for(i in 1:20){
  x <- knn(knn_data_both[,-30], knn_test[,-30], knn_data_both$Class, k=i)
  y[i] <- mean(knn_test$Class != x)
}
mink=which.min(y)

knn_both=knn(knn_data_both[,-30], knn_test[,-30], knn_data_both$Class, k=1, prob=TRUE)

##knn smote sampling
x<-NULL
y<-NULL
mink=NULL
for(i in 1:20){
  x <- knn(knn_data_smote[,-30], knn_test[,-30], knn_data_smote$Class, k=i)
  y[i] <- mean(knn_test$Class != x)
}
mink=which.min(y)

knn_smote=knn(knn_data_smote[,-30], knn_test[,-30], knn_data_smote$Class, k=mink, prob = TRUE)


##knn rose sampling
x<-NULL
y<-NULL
mink=NULL
for(i in 1:20){
  x <- knn(knn_data_rose[,-30], knn_test[,-30], knn_data_rose$Class, k=i)
  y[i] <- mean(knn_test$Class != x)
}
mink=which.min(y)
print(mink)
knn_rose=knn(knn_data_rose[,-30], knn_test[,-30], knn_data_rose$Class, k=mink, prob = TRUE)


##knn without sampling
x<-NULL
y<-NULL
mink=NULL
for(i in 1:20){
  x <- knn(knn_train[,-30], knn_test[,-30], knn_train$Class, k=i)
  y[i] <- mean(knn_test$Class != x)
}
mink=which.min(y)

knn_actual=knn(knn_train[,], knn_test, knn_train$Class, k=mink, prob = TRUE)


##Confusion matrix under sampling
knn_table_under<- table(knn_test$Class,knn_under,dnn=list('actual','predicted'))
knn_table_under
##Confusion matrix over sampling
knn_table_over<- table(knn_test$Class,knn_pred_over,dnn=list('actual','predicted'))
knn_table_over
##Confusion matrix both sampling
knn_table_both<- table(knn_test$Class,knn_both,dnn=list('actual','predicted'))
knn_table_both
##Confusion matrix smote sampling
knn_table_smote<- table(knn_test$Class,knn_smote,dnn=list('actual','predicted'))
knn_table_smote
##Confusion matrix rose sampling
knn_table_rose<- table(knn_test$Class,knn_pred_rose,dnn=list('actual','predicted'))
knn_table_rose
##Confusion matrix without sampling
knn_table_actual<- table(knn_test$Class,knn_pred_actual,dnn=list('actual','predicted'))
knn_table_actual


##Accuracy under sampling
knn_acc_under=(knn_table_under[1]+knn_table_under[4])/(knn_table_under[2]+knn_table_under[4]+knn_table_under[3]+knn_table_under[1])
knn_acc_under
##Accuracy over sampling
knn_acc_over=(knn_table_over[1]+knn_table_over[4])/(knn_table_over[2]+knn_table_over[4]+knn_table_over[3]+knn_table_over[1])
knn_acc_over
##Accuracy both sampling
knn_acc_both=(knn_table_both[1]+knn_table_both[4])/(knn_table_both[2]+knn_table_both[4]+knn_table_both[3]+knn_table_both[1])
knn_acc_both
##Accuracy smote sampling
knn_acc_smote=(knn_table_smote[1]+knn_table_smote[4])/(knn_table_smote[2]+knn_table_smote[4]+knn_table_smote[3]+knn_table_smote[1])
knn_acc_smote
##Accuracy rose sampling
knn_acc_rose=(knn_table_rose[1]+knn_table_rose[4])/(knn_table_rose[2]+knn_table_rose[4]+knn_table_rose[3]+knn_table_rose[1])
knn_acc_rose
##Accuracy without sampling
knn_acc_actual=(knn_table_actual[1]+knn_table_actual[4])/(knn_table_actual[2]+knn_table_actual[4]+knn_table_actual[3]+knn_table_actual[1])
knn_acc_actual

##Specificity under sampling

knn_spe_under=(knn_table_under[1]/(knn_table_under[1]+knn_table_under[3]))
knn_spe_under
knn_recall_under=(knn_table_under[4]/(knn_table_under[2]+knn_table_under[4]))
knn_recall_under
##Specificity over sampling
knn_spe_over=(knn_table_over[4]/(knn_table_over[1]+knn_table_over[3]))
knn_spe_over
knn_recall_over=(knn_table_over[4]/(knn_table_over[2]+knn_table_over[4]))
knn_recall_over
##Specificity both sampling
knn_spe_both=(knn_table_both[1]/(knn_table_both[1]+knn_table_both[3]))
knn_spe_both
knn_recall_both=(knn_table_both[4]/(knn_table_both[2]+knn_table_both[4]))
knn_recall_both
##Specificity smote sampling
knn_spe_smote=(knn_table_smote[1]/(knn_table_smote[1]+knn_table_smote[3]))
knn_spe_smote
knn_recall_smote=(knn_table_smote[4]/(knn_table_smote[2]+knn_table_smote[4]))
knn_recall_smote
##Specificity without sampling
knn_spe_actual=(knn_table_actual[1]/(knn_table_actual[1]+knn_table_actual[3]))
knn_spe_actual
knn_recall_actual=(knn_table_actual[4]/(knn_table_actual[2]+knn_table_actual[4]))
knn_recall_actual
##Specificity rose sampling
knn_spe_rose=(knn_table_rose[1]/(knn_table_rose[1]+knn_table_rose[3]))
knn_spe_rose
knn_recall_rose=(knn_table_rose[4]/(knn_table_rose[2]+knn_table_rose[4]))
knn_recall_rose


##ROCR under sampling



##ROCR smote sampling


library(ROCR)
accuracy.meas(knn_test[,30], as.numeric(knn_under))
knn_pred.under <- prediction( as.numeric(knn_under), knn_test[,30] )
knn_perf_under <- performance( knn_pred.under, "tpr", "fpr" )
knn_perf1_under <- performance( knn_pred.under, measure = "auc" )
knn_perf2_under <- performance( knn_pred.under, "prec", "rec" )
knn_auc_under <- knn_perf1_under@y.values[[1]]
knn_auc_under



accuracy.meas(knn_test[,30], as.numeric(knn_both))
knn_pred.both <- prediction( as.numeric(knn_both), knn_test[,30] )
knn_perf_both <- performance( knn_pred.both, "tpr", "fpr" )
knn_perf1_both <- performance( knn_pred.both, measure = "auc" )
knn_perf2_both <- performance( knn_pred.both, "prec", "rec" )
knn_auc_both <- knn_perf1_both@y.values[[1]]
knn_auc_both


##ROCR both sampling
accuracy.meas(knn_test[,30], as.numeric(knn_smote))
knn_pred.smote <- prediction( as.numeric(knn_smote), knn_test[,30] )
knn_perf_smote <- performance( knn_pred.smote, "tpr", "fpr" )
knn_perf1_smote <- performance( knn_pred.smote, measure = "auc" )
knn_perf2_smote <- performance( knn_pred.smote, "prec", "rec" )
knn_auc_smote <- knn_perf1_smote@y.values[[1]]
knn_auc_under

##ROCR over sampling
accuracy.meas(knn_test[,30], as.numeric(knn_over))
knn_pred.over <- prediction( as.numeric(knn_over), knn_test[,30] )
knn_perf_over <- performance( knn_pred.over, "tpr", "fpr" )
knn_perf1_over <- performance( knn_pred.over, measure = "auc" )
knn_perf2_over <- performance( knn_pred.over, "prec", "rec" )
knn_auc_over <- knn_perf1_over@y.values[[1]]
knn_auc_over


##ROCR rose sampling
accuracy.meas(knn_test[,30], as.numeric(knn_rose))
knn_pred.rose <- prediction( as.numeric(knn_rose), knn_test[,30] )
knn_perf_rose <- performance( knn_pred.rose, "tpr", "fpr" )
knn_perf1_rose <- performance( knn_pred.rose, measure = "auc" )
knn_perf2_rose <- performance( knn_pred.rose, "prec", "rec" )
knn_auc_rose <- knn_perf1_rose@y.values[[1]]
knn_auc_rose


##ROCR without sampling
accuracy.meas(knn_test[,30], as.numeric(knn_actual))
knn_pred.actual <- prediction( as.numeric(knn_actual), knn_test[,30] )
knn_perf_actual <- performance( knn_pred.actual, "tpr", "fpr" )
knn_perf1_actual <- performance( knn_pred.actual, measure = "auc" )
knn_perf2_actual <- performance( knn_pred.actual, "prec", "rec" )
knn_auc_actual <- knn_perf1_actual@y.values[[1]]
knn_auc_actual









##ROC curve
plot( knn_perf_actual , main="ROC curve", col = "blue")
abline(a = 0, b = 1, lty = 2,col="red")
plot( knn_perf_over, col = "green", add = TRUE)
plot( knn_perf_both , col = "yellow", add = TRUE)
plot( knn_perf_smote , col = "pink", add = TRUE)
plot( knn_perf_under ,main="ROC curve", col = "black", add = TRUE)
plot( knn_perf_rose , col = "purple", add = TRUE)

##Precision-Recall curve
plot(knn_perf2_actual , main="Precision-Recall curve", col = "blue")
abline(a = 0, b = 1, lty = 2,col="red")
plot( knn_perf2_over, col = "green", add = TRUE)
plot( knn_perf2_both , col = "yellow", add = TRUE)
plot( knn_perf2_smote , col = "pink", add = TRUE)
plot( knn_perf2_under , col = "black", add = TRUE)
plot( knn_perf2_rose , col = "purple", add = TRUE)