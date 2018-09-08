library(leaps)

library(ROSE)


creditcard <- read.csv("creditcard.csv")


### remove NA values


any(is.na(creditcard))
creditcard <- na.omit(creditcard)
summary(creditcard)

creditcard$Time <- NULL
regfit.fwd = regsubsets(Class~., data = creditcard, nvmax = 29, method = "forward")

summary(regfit.fwd)
reg.summary = summary(regfit.fwd)

which.min(reg.summary$bic)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)

regfit.bck = regsubsets(Class~., data = creditcard, nvmax = 29, method = "backward")

reg.bck.summary=summary(regfit.bck)

which.min(reg.bck.summary$bic)
which.max(reg.bck.summary$adjr2)
which.min(reg.bck.summary$cp)