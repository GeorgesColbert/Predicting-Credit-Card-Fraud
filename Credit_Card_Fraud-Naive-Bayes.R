nb_df=read.csv("creditcard.csv") 
col<- c(-1) 
nb_df<- nb_df[,col] 
set.seed(12345)
training_data <- sample(nrow(nb_df),0.7*nrow(nb_df)) 
nb_train <- nb_df[training_data,] 
nb_test<-nb_df[-training_data,]
library(ROSE)
prop.table(table(nb_df$Class))
prop.table(table(nb_train$Class))
prop.table(table(nb_test$Class))
##Under Sampling
nb_data_under=ovun.sample(Class ~ ., data = nb_train, method = "under", N = 684, seed = 1)$data
prop.table(table(nb_data_under$Class))
##Over Sampling
nb_data_over=ovun.sample(Class ~ ., data = nb_train, method="over", N=398044,seed = 1)$data
table(nb_data_over$Class)
##Both Sampling
nb_data_both=ovun.sample(Class ~ ., data = nb_train, method="both", p=0.5, N=1000,seed = 1)$data
table(nb_data_both$Class)
##Rose
nb_data_rose <- ROSE(Class ~ ., data = nb_train, seed = 12345)$data
table(nb_data_rose$Class)
##smote
library(DMwR)
nb_train$Class=as.factor(nb_train$Class)
nb_data_smote <- SMOTE(Class ~ ., data= nb_train, perc.over = 100, perc.under=200)
table(nb_data_smote$Class)





##NAIVE BIAS under sampling






library(e1071)


nb_under=naiveBayes(as.factor(Class)~.,data=nb_data_under)
nb_under
nb_pred_under1=predict(nb_under,newdata = nb_test, "raw")
nb_pred_under=predict(nb_under,newdata = nb_test)
nb_pred_under

##NAIVE BIAS over sampling
nb_over=naiveBayes(as.factor(Class)~.,data=nb_data_over)
nb_over
nb_pred_over1=predict(nb_over,newdata = nb_test, "raw")
nb_pred_over=predict(nb_over,newdata = nb_test)
nb_pred_over

##NAIVE BIAS both sampling
nb_both=naiveBayes(as.factor(Class)~.,data=nb_data_both)
nb_both
nb_pred_both1=predict(nb_both,newdata = nb_test, "raw")
nb_pred_both=predict(nb_both,newdata = nb_test)
nb_pred_both

##NAIVE BIAS smote sampling
nb_smote=naiveBayes(as.factor(Class)~.,data=nb_data_smote)
nb_smote
nb_pred_smote1=predict(nb_smote,newdata = nb_test, "raw")
nb_pred_smote=predict(nb_smote,newdata = nb_test)
nb_pred_smote

##NAIVE BIAS rose sampling
nb_rose=naiveBayes(as.factor(Class)~.,data=nb_data_rose)
nb_rose
nb_pred_rose1=predict(nb_rose,newdata = nb_test, "raw")
nb_pred_rose=predict(nb_rose,newdata = nb_test)
nb_pred_rose

##NAIVE BIAS without sampling
nb_actual=naiveBayes(as.factor(Class)~.,data=nb_train)
nb_actual
nb_pred_actual1=predict(nb_actual,newdata = nb_test, "raw")
nb_pred_actual=predict(nb_actual,newdata = nb_test)
nb_pred_actual















nb_table_under<- table(nb_test$Class,nb_pred_under,dnn=list('actual','predicted'))
nb_table_under
nb_table_over<- table(nb_test$Class,nb_pred_over,dnn=list('actual','predicted'))
nb_table_over
nb_table_both<- table(nb_test$Class,nb_pred_both,dnn=list('actual','predicted'))
nb_table_both
nb_table_smote<- table(nb_test$Class,nb_pred_smote,dnn=list('actual','predicted'))
nb_table_smote
nb_table_rose<- table(nb_test$Class,nb_pred_rose,dnn=list('actual','predicted'))
nb_table_rose
nb_table_actual<- table(nb_test$Class,nb_pred_actual,dnn=list('actual','predicted'))
nb_table_actual

##SPECIFICITY under sampling
nb_spe_under=(nb_table_under[1]/(nb_table_under[1]+nb_table_under[3]))
nb_spe_under
##SPECIFICITY over sampling
nb_spe_over=(nb_table_over[1]/(nb_table_over[1]+nb_table_over[3]))
nb_spe_over
##SPECIFICITY rose sampling
nb_spe_rose=(nb_table_rose[1]/(nb_table_rose[1]+nb_table_rose[3]))
nb_spe_rose
##SPECIFICITY both sampling
nb_spe_both=(nb_table_both[1]/(nb_table_both[1]+nb_table_both[3]))
nb_spe_both
##SPECIFICITY without sampling
nb_spe_actual=(nb_table_actual[1]/(nb_table_actual[1]+nb_table_actual[3]))
nb_spe_actual
##SPECIFICITY smote sampling
nb_spe_smote=(nb_table_smote[1]/(nb_table_smote[1]+nb_table_smote[3]))
nb_spe_smote

##ACCURACY under sampling
nb_acc_under=(nb_table_under[1]+nb_table_under[4])/(nb_table_under[2]+nb_table_under[4]+nb_table_under[3]+nb_table_under[1])
nb_acc_under
##ACCURACY over sampling
nb_acc_over=(nb_table_over[1]+nb_table_over[4])/(nb_table_over[2]+nb_table_over[4]+nb_table_over[3]+nb_table_over[1])
nb_acc_over
##ACCURACY both sampling
nb_acc_both=(nb_table_both[1]+nb_table_both[4])/(nb_table_both[2]+nb_table_both[4]+nb_table_both[3]+nb_table_both[1])
nb_acc_both
##ACCURACY smote sampling
nb_acc_smote=(nb_table_smote[1]+nb_table_smote[4])/(nb_table_smote[2]+nb_table_smote[4]+nb_table_smote[3]+nb_table_smote[1])
nb_acc_smote
##ACCURACY rose sampling
nb_acc_rose=(nb_table_rose[1]+nb_table_rose[4])/(nb_table_rose[2]+nb_table_rose[4]+nb_table_rose[3]+nb_table_rose[1])
nb_acc_rose
##ACCURACY without sampling
nb_acc_actual=(nb_table_actual[1]+nb_table_actual[4])/(nb_table_actual[2]+nb_table_actual[4]+nb_table_actual[3]+nb_table_actual[1])
nb_acc_actual



##ROCR and Precision Recall curve
library(ROCR)
accuracy.meas(nb_test[,30], as.numeric(nb_pred_under1[,2]))
nb_pred.under <- prediction( as.numeric(nb_pred_under), nb_test[,30] )
nb_perf_under <- performance( nb_pred.under, "tpr", "fpr" )
nb_perf1_under <- performance( nb_pred.under, measure = "auc" )
nb_perf2_under <- performance( nb_pred.under, "prec", "rec" )
nb_auc_under <- nb_perf1_under@y.values[[1]]
nb_auc_under

accuracy.meas(nb_test[,30], as.numeric(nb_pred_over1[,2]))
nb_pred.over <- prediction( as.numeric(nb_pred_over), nb_test[,30] )
nb_perf_over <- performance( nb_pred.over, "tpr", "fpr" )
nb_perf1_over <- performance( nb_pred.over, measure = "auc" )
nb_perf2_over <- performance( nb_pred.over, "prec", "rec" )
nb_auc_over <- nb_perf1_over@y.values[[1]]
nb_auc_over

accuracy.meas(nb_test[,30], as.numeric(nb_pred_both1[,2]))
nb_pred.both <- prediction( as.numeric(nb_pred_both), nb_test[,30] )
nb_perf_both <- performance( nb_pred.both, "tpr", "fpr" )
nb_perf1_both <- performance( nb_pred.both, measure = "auc" )
nb_perf2_both <- performance( nb_pred.both, "prec", "rec" )
nb_auc_both <- nb_perf1_both@y.values[[1]]
nb_auc_both

accuracy.meas(nb_test[,30], as.numeric(nb_pred_smote1[,2]))
nb_pred.smote <- prediction( as.numeric(nb_pred_smote), nb_test[,30] )
nb_perf_smote <- performance( nb_pred.smote, "tpr", "fpr" )
nb_perf1_smote <- performance( nb_pred.smote, measure = "auc" )
nb_perf2_smote<- performance( nb_pred.smote, "prec", "rec" )
nb_auc_smote <- nb_perf1_smote@y.values[[1]]
nb_auc_smote

accuracy.meas(nb_test[,30], as.numeric(nb_pred_actual1[,2]))
nb_pred.actual <- prediction( as.numeric(nb_pred_actual), nb_test[,30] )
nb_perf_actual <- performance( nb_pred.actual, "tpr", "fpr" )
nb_perf1_actual <- performance( nb_pred.actual, measure = "auc" )
nb_perf2_actual <- performance( nb_pred.actual, "prec", "rec" )
nb_auc_actual <- nb_perf1_actual@y.values[[1]]
nb_auc_actual


accuracy.meas(nb_test[,30], as.numeric(nb_pred_rose1[,2]))
nb_pred.rose <- prediction( as.numeric(nb_pred_rose), nb_test[,30] )
nb_perf_rose <- performance( nb_pred.rose, "tpr", "fpr" )
nb_perf1_rose <- performance( nb_pred.rose, measure = "auc" )
nb_perf2_rose <- performance( nb_pred.rose, "prec", "rec" )
nb_auc_rose <- nb_perf1_rose@y.values[[1]]
nb_auc_rose



plot( nb_perf_actual , main="ROC curve", col = "blue")
abline(a = 0, b = 1, lty = 2,col="red")
plot( nb_perf_over, col = "green", add = TRUE)
plot( nb_perf_both , col = "yellow", add = TRUE)
plot( nb_perf_smote , col = "pink", add = TRUE)
plot( nb_perf_under , col = "black", add = TRUE)
plot( nb_perf_rose , col = "purple", add = TRUE)


plot(nb_perf2_actual , main="Precision-Recall curve", col = "blue")
abline(a = 0, b = 1, lty = 2,col="red")
plot( nb_perf2_over, col = "green", add = TRUE)
plot( nb_perf2_both , col = "yellow", add = TRUE)
plot( nb_perf2_smote , col = "pink", add = TRUE)
plot( nb_perf2_under , col = "black", add = TRUE)
plot( nb_perf2_rose , col = "purple", add = TRUE)




