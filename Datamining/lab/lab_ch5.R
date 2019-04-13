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

#without family
glm.fit <- glm(mpg~horsepower, data = Auto)
coef(glm.fit) #lm과 같은 결과

lm.fit <- lm(mpg~horsepower, data = Auto)
coef(lm.fit)

library(boot)
dim(Auto)
cv.err <- cv.glm(data = Auto, glmfit = glm.fit)

cv.err$delta

# There are two delta values returned actually, the first is the one that you calculated above.
# The second is a corrected delta value with less bias1 than the straight MSE
# but in most cases they are quite similar to each other - and
# probably somewhat different from the MSE you calculated above!

cv.error <- rep(0,5)
for(i in 1:5){
  glm.fit <- glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
  }

cv.error

###3.k-Flod Cross-Validation###
set.seed(17)
cv.error.10 <- rep(0,10)

for(i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10

###4. The Bootstrap###
alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  
  return((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100, replace = T))

boot(Portfolio, alpha.fn, R = 100)


#Estimating the Accuracy of a Linear Regression Model
boot.fn <- function(data, index){
    return(coef(lm(mpg~horsepower,data = data, subset = index)))
  }

boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, replace = T))
boot.fn(Auto, sample(392, replace = T))

boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower, data = Auto))$coef


boot.fn <- function(data, index){
  
  coefficients(lm(mpg~horsepower + I(horsepower^2),
                  data = data, subset = index))
}

set.seed(1)
boot(Auto, boot.fn, 1000)

summary (lm(mpg~horsepower +I(horsepower ^2) ,data=Auto))$coef
