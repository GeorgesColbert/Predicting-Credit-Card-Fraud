rf_df=read.csv("creditcard.csv") 
col<- c(-1) 
rf_df<- rf_df[,col] 
set.seed(12345)
training_data <- sample(nrow(rf_df),0.7*nrow(rf_df)) 
rf_train <- rf_df[training_data,] 
rf_test<-rf_df[-training_data,]
library(ROSE)
##under sampling
rf_data_under=ovun.sample(Class ~ ., data = rf_train, method = "under", N = 684, seed = 1)$data
prop.table(table(rf_data_under$Class))
##over sampling
rf_data_over=ovun.sample(Class ~ ., data = rf_train, method="over", N=398044,seed = 1)$data
table(rf_data_over$Class)
##both sampling
rf_data_both=ovun.sample(Class ~ ., data = rf_train, method="both", p=0.5, N=1000,seed = 1)$data
table(rf_data_both$Class)
##rose sampling
rf_data_rose <- ROSE(Class ~ ., data = rf_train, seed = 12345)$data
table(rf_data_rose$Class)
##smote
library(DMwR)
rf_train$Class=as.factor(rf_train$Class)
rf_data_smote <- SMOTE(Class ~ ., data= rf_train, perc.over = 100, perc.under=200)
table(rf_data_smote$Class)

##RANDOMFOREST under sampling






library(randomForest)
rf_under=randomForest(as.factor(Class)~.,data=rf_data_under,mtry=6, importance=TRUE)
rf_pred_under = predict(rf_under,newdata=rf_test) 
##RANDOMFOREST over sampling
rf_over=randomForest(as.factor(Class)~.,data=rf_data_over,mtry=6, importance=TRUE)
rf_pred_over = predict(rf_over,newdata=rf_test) 
##RANDOMFOREST both sampling
rf_both=randomForest(as.factor(Class)~.,data=rf_data_both,mtry=6, importance=TRUE)
rf_pred_both = predict(rf_both,newdata=rf_test) 
##RANDOMFOREST smote sampling
rf_smote=randomForest(as.factor(Class)~.,data=rf_data_smote,mtry=6, importance=TRUE)
rf_pred_smote = predict(rf_smote,newdata=rf_test) 

##RANDOMFOREST rose sampling
rf_rose=randomForest(as.factor(Class)~.,data=rf_data_rose,mtry=6, importance=TRUE)
rf_pred_rose = predict(rf_rose,newdata=rf_test) 
##RANDOMFOREST without sampling
rf_actual=randomForest(as.factor(Class)~.,data=rf_train,mtry=6, importance=TRUE)
rf_pred_actual = predict(rf_actual,newdata=rf_test) 


##CONFUSION MATRIX under sampling






rf_table_under<- table(rf_test$Class,rf_pred_under,dnn=list('actual','predicted'))
rf_table_under
recall.under <- rf_table_under[2,1]/sum(rf_table_under[2,])
recall.under
##CONFUSION MATRIX over sampling
rf_table_over<- table(rf_test$Class,rf_pred_over,dnn=list('actual','predicted'))
rf_table_over
recall.over <- rf_table_over[2,1]/sum(rf_table_over[2,])
recall.over
##CONFUSION MATRIX both sampling
rf_table_both<- table(rf_test$Class,rf_pred_both,dnn=list('actual','predicted'))
rf_table_both
recall.both <- rf_table_both[2,1]/sum(rf_table_both[2,])
recall.both
##CONFUSION MATRIX smote sampling
rf_table_smote<- table(rf_test$Class,rf_pred_smote,dnn=list('actual','predicted'))
rf_table_smote
recall.smote <- rf_table_smote[2,1]/sum(rf_table_smote[2,])
recall.smote
##CONFUSION MATRIX rose sampling
rf_table_rose<- table(rf_test$Class,rf_pred_rose,dnn=list('actual','predicted'))
rf_table_rose
recall.rose <- rf_table_rose[2,1]/sum(rf_table_rose[2,])
recall.rose
##CONFUSION MATRIX without sampling
rf_table_actual<- table(rf_test$Class,rf_pred_actual,dnn=list('actual','predicted'))
rf_table_actual
recall.actual <- rf_table_actual[2,1]/sum(rf_table_actual[2,])
recall.actual


##ACCURACY under sampling

rf_acc_under=(rf_table_under[1]+rf_table_under[4])/(rf_table_under[2]+rf_table_under[4]+rf_table_under[3]+rf_table_under[1])
rf_acc_under
##ACCURACY over sampling
rf_acc_over=(rf_table_over[1]+rf_table_over[4])/(rf_table_over[2]+rf_table_over[4]+rf_table_over[3]+rf_table_over[1])
rf_acc_over
##ACCURACY both sampling
rf_acc_both=(rf_table_both[1]+rf_table_both[4])/(rf_table_both[2]+rf_table_both[4]+rf_table_both[3]+rf_table_both[1])
rf_acc_both
##ACCURACY smote sampling
rf_acc_smote=(rf_table_smote[1]+rf_table_smote[4])/(rf_table_smote[2]+rf_table_smote[4]+rf_table_smote[3]+rf_table_smote[1])
rf_acc_smote
##ACCURACY rose sampling
rf_acc_rose=(rf_table_rose[1]+rf_table_rose[4])/(rf_table_rose[2]+rf_table_rose[4]+rf_table_rose[3]+rf_table_rose[1])
rf_acc_rose
##ACCURACY without sampling
rf_acc_actual=(rf_table_actual[1]+rf_table_actual[4])/(rf_table_actual[2]+rf_table_actual[4]+rf_table_actual[3]+rf_table_actual[1])
rf_acc_actual

##SPECIFICITY under sampling
rf_spe_under=(rf_table_under[1]/(rf_table_under[1]+rf_table_under[3]))
rf_spe_under
##SPECIFICITY over sampling
rf_spe_over=(rf_table_over[1]/(rf_table_over[1]+rf_table_over[3]))
rf_spe_over
##SPECIFICITY both sampling
rf_spe_both=(rf_table_both[1]/(rf_table_both[1]+rf_table_both[3]))
rf_spe_both
##SPECIFICITY smote sampling
rf_spe_smote=(rf_table_smote[1]/(rf_table_smote[1]+rf_table_smote[3]))
rf_spe_smote
##SPECIFICITY without sampling
rf_spe_actual=(rf_table_actual[1]/(rf_table_actual[1]+rf_table_actual[3]))
rf_spe_actual
##SPECIFICITY rose sampling
rf_spe_rose=(rf_table_rose[1]/(rf_table_rose[1]+rf_table_rose[3]))
rf_spe_rose

##ROCR under sampling
library(ROCR)

rf_pred.under <- prediction( as.numeric(rf_pred_under), rf_test[,30] )
rf_perf_under <- performance( rf_pred.under, "tpr", "fpr" )
rf_perf1_under <- performance( rf_pred.under, measure = "auc" )
rf_perf2_under <- performance( rf_pred.under, "prec", "rec" )
rf_auc_under <- rf_perf1_under@y.values[[1]]
rf_auc_under

##ROCR over sampling

rf_pred.over <- prediction( as.numeric(rf_pred_over), rf_test[,30] )
rf_perf_over <- performance( rf_pred.over, "tpr", "fpr" )
rf_perf1_over <- performance( rf_pred.over, measure = "auc" )
rf_perf2_over <- performance( rf_pred.over, "prec", "rec" )
rf_auc_over <- rf_perf1_over@y.values[[1]]
rf_auc_over

##ROCR both sampling

rf_pred.both <- prediction( as.numeric(rf_pred_both), rf_test[,30] )
rf_perf_both <- performance( rf_pred.both, "tpr", "fpr" )
rf_perf1_both <- performance( rf_pred.both, measure = "auc" )
rf_perf2_both <- performance( rf_pred.both, "prec", "rec" )
rf_auc_both <- rf_perf1_both@y.values[[1]]
rf_auc_both

##ROCR smote sampling

rf_pred.smote <- prediction( as.numeric(rf_pred_smote), rf_test[,30] )
rf_perf_smote <- performance( rf_pred.smote, "tpr", "fpr" )
rf_perf1_smote <- performance( rf_pred.smote, measure = "auc" )
rf_perf2_smote<- performance( rf_pred.smote, "prec", "rec" )
rf_auc_smote <- rf_perf1_smote@y.values[[1]]
rf_auc_smote

##ROCR without sampling

rf_pred.actual <- prediction( as.numeric(rf_pred_actual), rf_test[,30] )
rf_perf_actual <- performance( rf_pred.actual, "tpr", "fpr" )
rf_perf1_actual <- performance( rf_pred.actual, measure = "auc" )
rf_perf2_actual <- performance( rf_pred.actual, "prec", "rec" )
rf_auc_actual <- rf_perf1_actual@y.values[[1]]
rf_auc_actual

##ROCR rose sampling
accuracy.meas(rf_test[,30], as.numeric(rf_pred_rose))
rf_pred.rose <- prediction( as.numeric(rf_pred_rose), rf_test[,30] )
rf_perf_rose <- performance( rf_pred.rose, "tpr", "fpr" )
rf_perf1_rose <- performance( rf_pred.rose, measure = "auc" )
rf_perf2_rose <- performance( rf_pred.rose, "prec", "rec" )
rf_auc_rose <- rf_perf1_rose@y.values[[1]]
rf_auc_rose





##ROC curve
plot( rf_perf_actual , main="ROC curve", col = "blue")
abline(a = 0, b = 1, lty = 2,col="red")
plot( rf_perf_over, col = "green", add = TRUE)
plot( rf_perf_both , col = "yellow", add = TRUE)
plot( rf_perf_smote , col = "pink", add = TRUE)
plot( rf_perf_under , col = "black", add = TRUE)
plot( rf_perf_rose , col = "purple", add = TRUE)

##Precision-Recall curve
plot(rf_perf2_actual , main="Precision-Recall curve", col = "blue")
abline(a = 0, b = 1, lty = 2,col="red")
plot( rf_perf2_over, col = "green", add = TRUE)
plot( rf_perf2_both , col = "yellow", add = TRUE)
plot( rf_perf2_smote , col = "pink", add = TRUE)
plot( rf_perf2_under , col = "black", add = TRUE)
plot( rf_perf2_rose , col = "purple", add = TRUE)


