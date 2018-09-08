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


########################################################
##########   Machine Learning Models   #################
########################################################



#########
##  Trees ##
########

#
#Without any specific sampling method
#

class_tree <- tree(Class ~.,data = df_train)
summary(class_tree)
#Plot the tree
plot(class_tree)
text(class_tree, pretty = 0)
title("No Sampling")
# Predicting the Test set results
pred_class_tree = predict(class_tree, newdata = df_test, type = 'class')
# Making the Confusion Matrix
cm = table(df_test[, 30], pred_class_tree)
cm


accuracy_class_tree <- (sum(as.numeric(df_test[, 30] == pred_class_tree)))/(nrow(df_test))
accuracy_class_tree


#Pruning the tree
# prune the tree 
set.seed(12345)
cv.class_tree <- cv.tree(class_tree, FUN = prune.misclass, K=10)
names(cv.class_tree)
cv.class_tree
plot(cv.class_tree$size,cv.class_tree$dev,type = 'b')

#Plot the pruned tree
prune.class_tree <- prune.misclass(class_tree,best=5)
plot(prune.class_tree)
text(prune.class_tree, pretty =1)
title("No sampling - Pruned Tree")

#Predicting test set results from pruned tree

pred_class_tree_pruned = predict(prune.class_tree, newdata = df_test, type = 'class')

#Confusion matrix for pruned tree
cm_prune = table(df_test[,30], pred_class_tree_pruned)
cm_prune

accuracy_class_tree_pruned <- (sum(as.numeric(df_test[, 30] == pred_class_tree_pruned)))/(nrow(df_test))
accuracy_class_tree_pruned

specificity.mat.tree <- as.matrix(cm_prune)
specificity.tree <- (specificity.mat.tree[1,1])/sum(specificity.mat.tree[1,])
specificity.tree


#
##OverSampling - Converting training dataset to twice the size of majority class
#

###Classification Tree
class_tree_os <- tree(Class ~.,data = df_train_over)
summary(class_tree_os)


#Plot the tree
plot(class_tree_os)
text(class_tree_os, pretty = 0)
title("Oversampling")

# Predicting the Test set results
pred_class_tree_os = predict(class_tree_os, newdata = df_test, type = 'class')

# Making the Confusion Matrix
cm_os = table(df_test[, 30], pred_class_tree_os)
cm_os

accuracy_class_tree_os <- (sum(as.numeric(df_test[, 30] == pred_class_tree_os)))/(nrow(df_test))
accuracy_class_tree_os


#Pruning the tree
# prune the tree 
cv.class_tree_os <- cv.tree(class_tree_os, FUN = prune.misclass, K=10)
names(cv.class_tree_os)
cv.class_tree_os
plot(cv.class_tree_os$size,cv.class_tree_os$dev,type = 'b')

#Plot the pruned tree
prune.class_tree_os <- prune.misclass(class_tree_os,best=8)
plot(prune.class_tree_os)
text(prune.class_tree_os, pretty =1)
title("Oversampling - Pruned Tree")

#Predicting test set results from pruned tree

pred_class_tree_pruned_os = predict(prune.class_tree_os, newdata = df_test, type = 'class')

#Confusion matrix for pruned tree
cm_prune_os = table(df_test[,30], pred_class_tree_pruned_os)
cm_prune_os

accuracy_class_tree_pruned_os <- (sum(as.numeric(df_test[, 30] == pred_class_tree_pruned_os)))/(nrow(df_test))
accuracy_class_tree_pruned_os

specificity.mat.os <- as.matrix(cm_prune_os)
specificity.os <- (specificity.mat.os[1,1])/sum(specificity.mat.os[1,])
specificity.os

#
##Under Sampling - Converting training dataset to twice the size of minority class
#

###Classification Tree
class_tree_us <- tree(Class ~.,data = df_train_under)
summary(class_tree_us)


#Plot the tree
plot(class_tree_us)
text(class_tree_us, pretty = 0)
title("Undersampling")

# Predicting the Test set results
pred_class_tree_us = predict(class_tree_us, newdata = df_test, type = 'class')

# Making the Confusion Matrix
cm_us = table(df_test[, 30], pred_class_tree_us)
cm_us

accuracy_class_tree_us <- (sum(as.numeric(df_test[, 30] == pred_class_tree_us)))/(nrow(df_test))
accuracy_class_tree_us


#Pruning the tree
# prune the tree 
cv.class_tree_us <- cv.tree(class_tree_us, FUN = prune.misclass, K=10)
names(cv.class_tree_us)
cv.class_tree_us
plot(cv.class_tree_us$size,cv.class_tree_us$dev,type = 'b')

#Plot the pruned tree
prune.class_tree_us <- prune.misclass(class_tree_us,best=6)
plot(prune.class_tree_us)
text(prune.class_tree_us, pretty =1)
title("Undersampling - Pruned Tree")

#Predicting test set results from pruned tree

pred_class_tree_pruned_us = predict(prune.class_tree_us, newdata = df_test, type = 'class')

#Confusion matrix for pruned tree
cm_prune_us = table(df_test[,30], pred_class_tree_pruned_us)
cm_prune_us

accuracy_class_tree_pruned_us <- (sum(as.numeric(df_test[, 30] == pred_class_tree_pruned_us)))/(nrow(df_test))
accuracy_class_tree_pruned_us

specificity.mat.us <- as.matrix(cm_prune_us)
specificity.us <- (specificity.mat.us[1,1])/sum(specificity.mat.us[1,])
specificity.us

#
## Both Over and Under Sampling
#

###Classification Tree
class_tree_both <- tree(Class ~.,data = df_train_both)
summary(class_tree_both)


#Plot the tree
plot(class_tree_both)
text(class_tree_both, pretty = 0)
title("Over + Under Sampling")
# Predicting the Test set results
pred_class_tree_both = predict(class_tree_both, newdata = df_test, type = 'class')

# Making the Confbothion Matrix
cm_both = table(df_test[, 30], pred_class_tree_both)
cm_both

accuracy_class_tree_both <- (sum(as.numeric(df_test[, 30] == pred_class_tree_both)))/(nrow(df_test))
accuracy_class_tree_both


#Pruning the tree
# prune the tree 
cv.class_tree_both <- cv.tree(class_tree_both, FUN = prune.misclass, K=10)
names(cv.class_tree_both)
cv.class_tree_both
plot(cv.class_tree_both$size,cv.class_tree_both$dev,type = 'b')

#Plot the pruned tree
prune.class_tree_both <- prune.misclass(class_tree_both,best=7)
plot(prune.class_tree_both)
text(prune.class_tree_both, pretty =1)
title("Over + Under Sampling - Pruned Tree")


#Predicting test set results from pruned tree

pred_class_tree_pruned_both = predict(prune.class_tree_both, newdata = df_test, type = 'class')

#Confbothion matrix for pruned tree
cm_prune_both = table(df_test[,30], pred_class_tree_pruned_both)
cm_prune_both

accuracy_class_tree_pruned_both <- (sum(as.numeric(df_test[, 30] == pred_class_tree_pruned_both)))/(nrow(df_test))
accuracy_class_tree_pruned_both

specificity.mat.both <- as.matrix(cm_prune_both)
specificity.both <- (specificity.mat.both[1,1])/sum(specificity.mat.both[1,])
specificity.both


##########
##ROSE
##########

###Classification Tree
class_tree_rose <- tree(Class ~.,data = df_train_rose)
summary(class_tree_rose)


#Plot the tree
plot(class_tree_rose)
text(class_tree_rose, pretty = 0)
title("ROSE")


# Predicting the Test set results
pred_class_tree_rose = predict(class_tree_rose, newdata = df_test, type = 'class')

# Making the Confroseion Matrix
cm_rose = table(df_test[, 30], pred_class_tree_rose)
cm_rose

accuracy_class_tree_rose <- (sum(as.numeric(df_test[, 30] == pred_class_tree_rose)))/(nrow(df_test))
accuracy_class_tree_rose


#Pruning the tree
# prune the tree 
cv.class_tree_rose <- cv.tree(class_tree_rose, FUN = prune.misclass, K=10)
names(cv.class_tree_rose)
cv.class_tree_rose
plot(cv.class_tree_rose$size,cv.class_tree_rose$dev,type = 'b')

#Plot the pruned tree
prune.class_tree_rose <- prune.misclass(class_tree_rose,best=7)
plot(prune.class_tree_rose)
text(prune.class_tree_rose, pretty =1)
title("ROSE - Pruned Tree")

#Predicting test set results from pruned tree

pred_class_tree_pruned_rose = predict(prune.class_tree_rose, newdata = df_test, type = 'class')

#Confroseion matrix for pruned tree
cm_prune_rose = table(df_test[,30], pred_class_tree_pruned_rose)
cm_prune_rose

accuracy_class_tree_pruned_rose <- (sum(as.numeric(df_test[, 30] == pred_class_tree_pruned_rose)))/(nrow(df_test))
accuracy_class_tree_pruned_rose

specificity.mat.rose <- as.matrix(cm_prune_rose)
specificity.rose <- (specificity.mat.rose[1,1])/sum(specificity.mat.rose[1,])
specificity.rose


#
##SMOTE
#

###Classification Tree
class_tree_smote <- tree(Class ~.,data = df_train_smote)
summary(class_tree_smote)


#Plot the tree
plot(class_tree_smote)
text(class_tree_smote, pretty = 0)
title("SMOTE")

# Predicting the Test set results
pred_class_tree_smote = predict(class_tree_smote, newdata = df_test, type = 'class')

# Making the Confsmoteion Matrix
cm_smote = table(df_test[, 30], pred_class_tree_smote)
cm_smote

accuracy_class_tree_smote <- (sum(as.numeric(df_test[, 30] == pred_class_tree_smote)))/(nrow(df_test))
accuracy_class_tree_smote


#Pruning the tree
# prune the tree 
cv.class_tree_smote <- cv.tree(class_tree_smote, FUN = prune.misclass, K=10)
names(cv.class_tree_smote)
cv.class_tree_smote
plot(cv.class_tree_smote$size,cv.class_tree_smote$dev,type = 'b')

#Plot the pruned tree
prune.class_tree_smote <- prune.misclass(class_tree_smote,best=7)
plot(prune.class_tree_smote)
text(prune.class_tree_smote, pretty =1)
title("SMOTE - Pruned Tree")

#Predicting test set results from pruned tree

pred_class_tree_pruned_smote = predict(prune.class_tree_smote, newdata = df_test, type = 'class')

#Confusion matrix for pruned tree
cm_prune_smote = table(df_test[,30], pred_class_tree_pruned_smote)
cm_prune_smote

accuracy_class_tree_pruned_smote <- (sum(as.numeric(df_test[, 30] == pred_class_tree_pruned_smote)))/(nrow(df_test))
accuracy_class_tree_pruned_smote


specificity.mat.smote <- as.matrix(cm_prune_smote)
specificity.smote <- (specificity.mat.smote[1,1])/sum(specificity.mat.smote[1,])
specificity.smote


#########################
##ROC Curve
#########################


###
## Trees
###


##Whole Data
prob.dt <-predict(class_tree, newdata = df_test, prob = TRUE)
accuracy.meas(df_test[,30], prob.dt[,2])
pred <- prediction( prob.dt[,2], df_test[,30] )
perf <- performance( pred, "tpr", "fpr" )
perf1 <- performance( pred, measure = "auc" )
perf2 <- performance( pred, "prec", "rec" )
auc <- perf1@y.values[[1]]
auc


##Both Over and Under Sampling
prob.dt.both <-predict(prune.class_tree_both, newdata = df_test, prob = TRUE)
accuracy.meas(df_test[,30], prob.dt.both[,2])
pred.both <- prediction( prob.dt.both[,2], df_test[,30] )
perf.both <- performance( pred.both, "tpr", "fpr" )
perf1.both <- performance( pred.both, measure = "auc" )
perf2.both <- performance( pred.both, "prec", "rec" )
auc.both <- perf1.both@y.values[[1]]
auc.both



###Under Sampling
prob.dt.us <-predict(prune.class_tree_us, newdata = df_test, prob = TRUE)
accuracy.meas(df_test[,30], prob.dt.us[,2])
pred.us <- prediction( prob.dt.us[,2], df_test[,30] )
perf.us <- performance( pred.us, "tpr", "fpr" )
perf1.us <- performance( pred.us, measure = "auc" )
perf2.us <- performance( pred.us, "prec", "rec" )
auc.us <- perf1.us@y.values[[1]]
auc.us

###Over Sampling
prob.dt.os <-predict(prune.class_tree_os, newdata = df_test, prob = TRUE)
accuracy.meas(df_test[,30], prob.dt.os[,2])
pred.os <- prediction( prob.dt.os[,2], df_test[,30] )
perf.os <- performance( pred.os, "tpr", "fpr" )
perf1.os <- performance( pred.os, measure = "auc" )
perf2.os <- performance( pred.os, "prec", "rec" )
auc.os <- perf1.os@y.values[[1]]
auc.os


###ROSE
prob.dt.rose <-predict(prune.class_tree_rose, newdata = df_test, prob = TRUE)
accuracy.meas(df_test[,30], prob.dt.rose[,2])
pred.rose <- prediction( prob.dt.rose[,2], df_test[,30] )
perf.rose <- performance( pred.rose, "tpr", "fpr" )
perf1.rose <- performance( pred.rose, measure = "auc" )
perf2.rose <- performance( pred.rose, "prec", "rec" )
auc.rose <- perf1.rose@y.values[[1]]
auc.rose


###SMOTE
prob.dt.smote <-predict(prune.class_tree_smote, newdata = df_test, prob = TRUE)
accuracy.meas(df_test[,30], prob.dt.smote[,2])
pred.smote <- prediction( prob.dt.smote[,2], df_test[,30] )
perf.smote <- performance( pred.smote, "tpr", "fpr" )
perf1.smote <- performance( pred.smote, measure = "auc" )
perf2.smote <- performance( pred.smote, "prec", "rec" )
auc.smote <- perf1.smote@y.values[[1]]
auc.smote



#Plot ROC on same graph
plot( perf , main="ROC curve - Trees", col = "blue")
abline(a = 0, b = 1, lty = 2,col="red")
plot( perf.os, col = "green", add = TRUE)
plot( perf.us , col = "black", add = TRUE)
plot( perf.both , col = "yellow", add = TRUE)
plot( perf.rose , col = "purple", add = TRUE)
plot( perf.smote , col = "pink", add = TRUE)




