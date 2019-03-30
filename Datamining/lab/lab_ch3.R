###1.Libraries###
library(MASS)
library(ISLR)

###2.Simple Linear Regression###
fix(Boston)
names(Boston)

lm.fit = lm(medv ~ lstat) #error
lm.fit = lm(medv ~ lstat, data = Boston)

attach(Boston)
lm.fit <- lm(medv ~ lstat) #after attach
lm.fit
summary(lm.fit)

names(lm.fit) #linear regression의 항목들 출력
confint(lm.fit) #각 계수별 CI

#given value of lstat, CI
predict(lm.fit, data.frame(lstat=c(5,10,15)),
        interval = 'confidence')
#특정 값을 가진 obs의 평균 추정. 오차항 고려 x

predict(lm.fit, data.frame(lstat=c(5,10,15)),
        interval = 'prediction')
#특정 값을 가진 obs가 주어졌을 때, 오차까지 고려한 경우. 위의 값보다 구간이 넓다.

plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = 'red')

plot(lstat, medv, col = 'red')
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = '+')

#various point characters
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(2,2))
plot(lm.fit)

#residual plots
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit)) # leverage statistics
which.max(hatvalues(lm.fit))

###3.Multiple Linear Regression###
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

lm.fit <- lm(medv ~., data = Boston)
summary(lm.fit)

library(car)
vif(lm.fit)

lm.fit <- lm(medv~.-age, data = Boston)
summary(lm.fit)
lm.fit1 <- update(lm.fit, ~.-age) #update

###4.Interaction Terms###
summary(lm(medv~lstat*age, data = Boston))

###5.Non-linear Transformations of the Predictors###
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

lm.fit <- lm(medv~lstat)
anova(lm.fit, lm.fit2)

par(mfrow = c(2,2))
plot(lm.fit2)

lm.fit5 = lm(medv~poly(lstat,5))
summary(lm.fit5)

summary(lm(medv~log(rm)), data = Boston)

###6.Qualitative Predictor###
fix(Carseats)
names(Carseats)
summary(Carseats)
lm.fit <- lm(Sales~.+Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)

###7.Writing Functions###
LoadLibraries <- function(){
  library(MASS)
  library(ISLR)
  print('The libraries have been loaded.')
}

LoadLibraries()