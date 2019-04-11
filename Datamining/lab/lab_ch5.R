setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Datamining/lab")

###1.The Validation Set Approach###
library(ISLR)
set.seed(1)
train <- sample(392, 196)
attach(Auto)

lm.fit <-lm(mpg~horsepower, data = Auto, subset = train)
#test MSE
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
#test MSE
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
#test MSE
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

#choose a different training set instead
set.seed(2)
train <- sample(392,196)

lm.fit <-lm(mpg~horsepower, data = Auto, subset = train)
#test MSE
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
#test MSE
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
#test MSE
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

###2.Leave-One-Out Cross-Validation(LOOCV) ###
