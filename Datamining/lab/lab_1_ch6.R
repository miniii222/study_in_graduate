#Subset selection Methods


####1.Best Subset Selection###
library(ISLR)
fix(HItters)
names(Hitters)

#NA check
sum(is.na(Hitters$Salary))
#remove NAs
Hitters <- na.omit(Hitters)

dim(Hitters)
sum(is.na(Hitters))

library(leaps)
regfit.full <- regsubsets(Salary~., data = Hitters)
summary(regfit.full)

#max number of variables : 19
regfit.full <- regsubsets(Salary~., data = Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
names(reg.summary)


reg.summary$rsq

par(mfrow = c(2,2))

#RSS
plot(reg.summary$rss, xlab = 'Number of Variables', ylab = 'RSS',
     type = 'l')

#Adjusted Rsquare
plot(reg.summary$adjr2, xlab = 'Number of Variables',
     ylab = 'Adjusted RSq', type = 'l')
###adjr^2 max
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = 'red', cex = 2, pch = 20)

#Cp
plot(reg.summary$cp, xlab = 'Number of Variables',
     ylab = 'Cp', type = 'l')
#cp min
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = 'red', cex = 2, pch = 20)

#BIC
plot(reg.summary$bic, xlab = 'Number of Variables',
     ylab = 'BIC', type = 'l')
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col = 'red', cex = 2, pch = 20)

plot(regfit.full, scale = 'r2')
plot(regfit.full, scale = 'adjr2')
plot(regfit.full, scale = 'Cp')
plot(regfit.full, scale = 'bic')

coef(regfit.full, 6)

###2.Forward and Backward Stepwise Selection
#forward
regfit.fwd <- regsubsets(Salary~., data = Hitters,
                         nvmax = 19, method = 'forward')
summary(regfit.fwd)

#backward
regfit.bwd <- regsubsets(Salary~., data = Hitters,
                         nvmax = 19, method = 'backward')
summary(regfit.fwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.fwd, 7)

#backward, forward selection same / best subset selection different

###3.Choosing Among Models Using the validation set approach and cv###
set.seed(1)
train <- sample(c(T,F), nrow(Hitters), rep = T)
test <- !train

regfit.best <- regsubsets(Salary~., data = Hitters[train,],
                          nvmax = 19)
#building 'X' matrix
test.mat <- model.matrix(Salary~., data = Hitters[test,])

val.errors <- rep(NA,19)
#id는 변수의 개수를 나타
for(i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[,names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred) ^2)
}

val.errors
#best rmse
which.min(val.errors)

coef(regfit.best, 10)

predict.regsubsets <- function(object, newdata,id,...){
  
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  
  mat[,xvars]%*%coefi
}

k <-10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = T)
cv.errors <- matrix(NA, k,19,
                   dimnames = list(NULL, paste(1:19)))

for(j in 1:k){
  best.fit <- regsubsets(Salary~., data = Hitters[folds!=j,],
                         nvmax = 19)
  for(i in 1:19){
    pred <- predict(best.fit, Hitters[folds == j,], id= i)
    cv.errors[j,i] <- mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

mean.cv.errors <- colMeans(cv.errors)
mean.cv.errors

par(mfrow = c(1,1))
plot(mean.cv.errors, type = 'b')
which.min(mean.cv.errors)

reg.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best,11)
