library(ISLR)

#excercise 6.9
data("College")

#(a)
#train, test 7:3 split
set.seed(0610)
train.ind <- sample(nrow(College), nrow(College)*0.6)
train <- College[train.ind, ]
test <- College[-train.ind,]


#(b)
lm1 <- lm(Apps~., data = train)
summary(lm1)

rmse <- function(pred, true){
  sqrt(mean((pred - true)^2) )
}

lm1.pred <- predict(lm1, test)
rmse(lm1.pred, test$Apps)


#(c)
library(glmnet)
train.mat <- model.matrix(Apps~., data = train)
test.mat <- model.matrix(Apps~., data = test)

grid <- 10 ^ seq(4, -2, length = 100)
cv.ridge1 <- cv.glmnet(train.mat, train$Apps, alpha = 0,
                       lambda = grid, thresh = 1e-12)
bestlam.ridge <- cv.ridge1$lambda.min
bestlam.ridge

fit.ridge <- glmnet(train.mat, train$Apps, alpha = 0, lambda = grid,
                    thresh = 1e-12)
ridge1 <- glmnet(train.mat, train$Apps, alpha = 0, lambda = bestlam.ridge,
                 thresh = 1e-12)
coef(ridge1)

pred.ridge <- predict(ridge1, test.mat)
rmse(pred.ridge, test$Apps)

#(d)
cv.lasso1 <- cv.glmnet(train.mat, train$Apps, lambda = grid, thresh = 1e-12,
                       alpha = 1)
bestlam.lasso <- cv.lasso1$lambda.min
bestlam.lasso

lasso1 <- glmnet(train.mat, train$Apps,
                    lambda = bestlam.lasso, thresh = 1e-12)
coef(lasso1)

lasso.pred <- predict(lasso1, test.mat)
rmse(lasso.pred, test$Apps)

predict(lasso1, s = bestlam.lasso, type = "coefficients")

#(e)
library(pls)
fit.pcr <- pcr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
validationplot(fit.pcr, val.type = "MSEP")

pred.pcr <- predict(fit.pcr, test, ncomp = 10)
rmse(pred.pcr, test$Apps)

#(f)
fit.pls <- plsr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
validationplot(fit.pls, val.type = "MSEP")

pred.pls <- predict(fit.pls, test, ncomp = 10)
rmse(pred.pls, test$Apps)

#(g)
r2 <- function(pred){
  test.avg <- mean(test$Apps)
  
  1 - mean((pred - test$Apps)^2) / mean((test.avg - test$Apps)^2)
  
}

r2(lm1.pred)
r2(pred.ridge)
r2(lasso.pred)
r2(pred.pcr)
r2(pred.pls)

#excercise 6.11
library(MASS)
data(Boston) #predict crime rate

#(a)

set.seed(100)
dim(Boston)

#5-fold
k = 5
folds <- sample(k, nrow(Boston), replace = TRUE,prob = rep(0.2,k))

#1.linear regression
cv.err.lr <- c()
for(i in 1:k){
  
  lr <- lm(crim ~., data = Boston[folds != i,])
  lr.pred <- predict(lr, Boston[folds == i,])
  cv.err.lr[i] <- rmse(lr.pred, Boston[folds == i, 'crim'])
}

mean(cv.err.lr)

#2.best subset
cv.err.bs <- matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))

for (j in 1:k) {
  
  best.fit <- regsubsets(crim ~ ., data = Boston[folds != j, ], nvmax = 13)
  
  for(i in 1:13){
    coefi <- coef(best.fit, id = i)
    test.mat <- model.matrix(crim~., data = Boston[folds == j, ])
    best.pred <- test.mat[,names(coefi)] %*% coefi
    cv.err.bs[j,i] <- rmse(best.pred, Boston[folds == j,'crim'])
  }
}

mean.cv.errors <- colMeans(cv.err.bs)
plot(mean.cv.errors, type = "b", xlab = "Number of variables", ylab = "CV error")
points(which.min(mean.cv.errors),min(mean.cv.errors), pch = 16)
which.min(mean.cv.errors)

#3.ridge
set.seed(100)
Boston.mat <- model.matrix(crim~., data = Boston)
cv.ridge <- cv.glmnet(Boston.mat, Boston$crim, alpha = 0,
                       lambda = grid, thresh = 1e-12)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge

#4.lasso
cv.lasso <- cv.glmnet(Boston.mat, Boston$crim, alpha = 1,
                      lambda = grid, thresh = 1e-12)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

cv.err.reg <- matrix(NA, nrow = k, ncol = 2)

for(i in 1:k){
  
  
  ridge1 <- glmnet(Boston.mat[folds!=i, ], Boston[folds != i,'crim'],
                   lambda = bestlam.ridge, thresh = 1e-12,
                   alpha = 0)
  lasso1 <- glmnet(Boston.mat[folds!=i, ], Boston[folds != i,'crim'],
                   lambda = bestlam.lasso, thresh = 1e-12)
  
  ridge.pred <- predict(ridge1, newx = Boston.mat[folds==i, ])
  lasso.pred <- predict(lasso1, newx = Boston.mat[folds==i, ])
  
  #ridge cv error
  cv.err.reg[i,1] <- rmse(ridge.pred, Boston[folds ==i, 'crim'])
  cv.err.reg[i,2] <- rmse(lasso.pred, Boston[folds ==i, 'crim'])
}

colMeans(cv.err.reg)
