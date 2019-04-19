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
data(Boston)
