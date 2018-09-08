
###Logistic Regression

log.spec <- c(  0.999,
                0.980,
                0.898,
                0.980,
                0.989)

barplot(log.spec,main = "Logistic Regression Method Used - AUC",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)

log.recall <- c(  0.633,
                  0.887,
                  0.927,
                  0.887,
                  0.847)

barplot(log.recall,main = "Logistic Regression Method Used - AUC",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)

log.AUC <- c( 0.944,
              0.97,
              0.913,
              0.971,
              0.969)

barplot(log.AUC,main = "Logistic Regression Method Used - AUC",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)

###Trees

tree.AUC <- c( 0.890,  0.941,0.913, 0.947,0.914,0.945)

barplot(tree.AUC,main = "Decision Tree Method Used - AUC",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)

tree.Spec <- c(  0.999,
                 0.970,
                 0.972,
                 0.971,
                 0.972)

barplot(tree.Spec,main = "Decision Tree Method Used - AUC",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


tree.Recall <- c(  0.774,
                   0.790,
                   0.806,
                   0.793,
                   0.805,
                   0.833)

barplot(tree.Recall,main = "Decision Tree Method Used - AUC",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)
###For Neural Networks
NN.AUC <- c(  0.998,
              0.500,
              0.494,
              0.999,
              0.535,
              0.989 )
              

barplot(NN.AUC,main = "Neural Network Method Used - AUC",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


NN.Recall <- c(   0.998,
                  0.499,
                  0.498,
                  0.982,
                  0.529,
                  0.989
)


barplot(NN.Recall,main = "Neural Network Method Used - Recall",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


NN.Spec <- c(    0.998,
                 0.499,
                 0.519,
                 1.0,
                 0.947,
                 0.989
                 
)


barplot(NN.Spec,main = "Neural Network Method Used - Specificity",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


###For Naive Bayes

NB.AUC <- c(   0.883,
               0.894,
               0.887,
               0.887,
               0.884,
               0.892
)


barplot(NB.AUC,main = "Naive Bayes Method Used - AUC",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


NB.Recall <- c(    0.787,
                   0.813,
                   0.807,
                   0.800,
                   0.787,
                   0.820
                   
)


barplot(NB.Recall,main = "Naive Bayes Method Used - Recall",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


NB.Spec <- c(    0.999,
                 0.999,
                 0.999,
                 0.999,
                 0.999,
                 0.999
)


barplot(NB.Spec,main = "Naive Bayes Method Used - Specificity",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


###For Boosting

Boost.AUC <- c( 0.913,
                0.965,
                0.979,
                0.981,
                0.945,
                0.997)


barplot(Boost.AUC,main = "Boosting - AUC",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


Boost.Recall <- c(    0.825,
                      0.831,
                      0.880,
                      0.867,
                      0.847,
                      1
                      
                   
)


barplot(Boost.Recall,main = "Boosting - Recall",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


Boost.Spec <- c(    0.999,
                 0.999,
                 0.999,
                 0.999,
                 0.999,
                 0.999
)


barplot(Boost.Spec,main = "Boosting - Specificity",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


###Random Forest

RF.AUC <- c(  0.883,
              0.894,
              0.887,
              0.887,
              0.884,
              0.892)


barplot(RF.AUC,main = "Random Forest - AUC",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


RF.Recall <- c(     0.787,
                    0.813,
                    0.807,
                    0.800,
                    0.787,
                    0.820)
                    


barplot(RF.Recall,main = "Random Forest - Recall",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


RF.Spec <- c(    0.999,
                    0.999,
                    0.999,
                    0.999,
                    0.999,
                    0.999
)


barplot(RF.Spec,main = "Random Forest - Specificity",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)



###KNN

KNN.AUC <- c(  0.844,
               0.845,
               0.881,
               0.876,
               0.866,
               0.880
)


barplot(RF.AUC,main = "K Nearest Neighbors - AUC",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)


KNN.Recall <- c(     0.774,
                     0.790,
                     0.806,
                     0.793,
                     0.805,
                     0.833
                     )


barplot(RF.Recall,main = "K Nearest Neighbors - Recall",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)

KNN.Spec <- c(    0.999,
                 0.999,
                 0.999,
                 0.999,
                 0.999,
                 0.999
)


barplot(RF.Spec,main = "K Nearest Neighbors - Specificity",
        xlab = "Method",
        ylab = "Performance Measure",
        names.arg = c("NS", "OS", "US", "Both", "ROSE", "SMOTE"),
        col=c(4,3,1,7,6,"Pink"),
        horiz = FALSE)

