#4
library(e1071)
set.seed(10)

x <- rnorm(100)
y <- 4 * x^2 + 1 + rnorm(100)
class <- sample(100, 50)
y[class] <- y[class] + 3
y[-class] <- y[-class] - 3
plot(x[class], y[class], col = "red", xlab = "X", ylab = "Y", ylim = c(-6, 30))
points(x[-class], y[-class], col = "blue")

z<-rep(NA,100)
z[class] <- 1; z[-class] <- -1

dt <- data.frame(x = x, y = y, z = as.factor(z))

train.ind <- sample(100,70)
train <- dt[train.ind,]
test <- dt[-train.ind,]

#linear
svmfit.linear <- svm(z~., data = dt,kernel = 'linear')

##train
pred.train <- predict(svmfit.linear, train)
table(pred = pred.train, true = train$z)
plot(svmfit.linear, train)
##test
pred.test <- predict(svmfit.linear, test)
table(pred = pred.test, true = test$z)
plot(svmfit.linear, test)

#polynomial
svmfit <- svm(z~., data = dt,kernel = 'polynomial')

##train
pred.train <- predict(svmfit, train)
table(pred = pred.train, true = train$z)
plot(svmfit, train)
##test
pred.test <- predict(svmfit, test)
table(pred = pred.test, true = test$z)
plot(svmfit, test)

#radial
svmfit <- svm(z~., data = dt,kernel = 'radial')

##train
pred.train <- predict(svmfit, train)
table(pred = pred.train, true = train$z)
plot(svmfit, train)
##test
pred.test <- predict(svmfit, test)
table(pred = pred.test, true = test$z)
plot(svmfit, test)


#5
##(a)
set.seed(20)
x1 <- runif(500)-0.5
x2 <- runif(500)-0.5
y <-1*(x1^2-x2^2 > 0)
dt <- data.frame(y = as.factor(y), x1 = x1, x2 = x2)

##(b)
plot(x1,x2, col = y+2, pch = y+2, xlab = 'x1', ylab = 'x2')

##(c)
lr1 <- glm(y~x1+x2, family = 'binomial', data = dt)
summary(lr1)

##(d)
lr1.pred <- predict(lr1, dt, 'response')
lr1.pred <- ifelse(lr1.pred > 0.5, 1,0)
plot(x1,x2, col = lr1.pred+2, pch = lr1.pred+2, xlab = 'x1', ylab = 'x2')
table(lr1.pred, dt$y)

##(e)
lr2 <- glm(y~x1+x2+I(x1*x2)+I(x1^2)+I(x2^3),
           family = 'binomial', data = dt)
summary(lr2)

##(f)
lr2.pred <- predict(lr2, dt, 'response')
lr2.pred <- ifelse(lr2.pred > 0.5, 1,0)
plot(x1,x2, col = lr2.pred+2, pch = lr2.pred+2, xlab = 'x1', ylab = 'x2')
table(lr2.pred, dt$y)

##(h)
svm1 <- svm(y ~., dt, kernel = "radial", gamma = 1)
svm1.pred <- predict(svm1, dt)

plot(svm1, dt)
table(svm1$fitted, dt$y)

#8
library(ISLR)
data(OJ)
dim(OJ)

##(a)
set.seed(5)
train.ind <-sample(nrow(OJ), 800)
train<- OJ[train.ind,]
test <- OJ[-train.ind,]

##(b) linear kernel
svm1 <- svm(Purchase ~., data = train, kernel = 'linear',cost = 0.01)
summary(svm1)

##(c)
#train data
train.pred <- predict(svm1, train)
table(train.pred, train$Purchase)

#test data
test.pred <- predict(svm1, test)
table(test.pred, test$Purchase)

##(d)
svm.tune <- tune(svm, Purchase~., data = train, kernel = 'linear',
                 ranges = list(cost = seq(0.01,10.2,0.4)))
summary(svm.tune)

##(e)
svm.best <- svm.tune$best.model

#train data
train.pred <- predict(svm.best, train)
table(train.pred, train$Purchase)

#test data
test.pred <- predict(svm.best, test)
table(test.pred, test$Purchase)



##(f) radial kernel
svm2 <- svm(Purchase ~., data = train, kernel = 'radial')
summary(svm2)

#train data
train.pred <- predict(svm2, train)
table(train.pred, train$Purchase)

#test data
test.pred <- predict(svm2, test)
table(test.pred, test$Purchase)

svm.tune <- tune(svm, Purchase~., data = train, kernel = 'radial',
                 ranges = list(cost = seq(0.01,5,0.4),
                               gamma = c(0.1,0.3,0.5,0.7,1,3)))
summary(svm.tune)

svm.best <- svm.tune$best.model

#train data
train.pred <- predict(svm.best, train)
table(train.pred, train$Purchase)

#test data
test.pred <- predict(svm.best, test)
table(test.pred, test$Purchase)

##(g) polynomial kernel
svm3 <- svm(Purchase ~., data = train, kernel = 'polynomial', degree = 2)
summary(svm3)

#train data
train.pred <- predict(svm3, train)
table(train.pred, train$Purchase)

#test data
test.pred <- predict(svm3, test)
table(test.pred, test$Purchase)

svm.tune <- tune(svm, Purchase~., data = train, kernel = 'polynomial',
                 degree = 2,
                 ranges = list(cost = seq(0.01,5,0.4),
                               gamma = c(0.1,0.3,0.5,0.7,1,3)))
summary(svm.tune)

svm.best <- svm.tune$best.model

#train data
train.pred <- predict(svm.best, train)
table(train.pred, train$Purchase)

#test data
test.pred <- predict(svm.best, test)
table(test.pred, test$Purchase)

##(h)