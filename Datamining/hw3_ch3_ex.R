setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Datamining/hw")
library(ISLR)

#Exercise 3.8
data(Auto); attach(Auto)

##(a)
lm1 <- lm(mpg~horsepower, data = Auto)
summary(lm1)

predict(lm1, data.frame(horsepower = 98))
predict(lm1, data.frame(horsepower = 98),interval = 'prediction')
predict(lm1, data.frame(horsepower = 98),interval = 'confidence')
##(b)
plot(horsepower, mpg)
abline(lm1, col = 'red')

##(c)
par(mfrow = c(2,2))
plot(lm1)

#Exercise 3.9
##(a)
plot(Auto)

##(b)
cor(Auto[, -9])

##(c)
lm1 <- lm(mpg~.-name, data = Auto)
summary(lm1)

##(d)
plot(lm1)

##(e)
fit.int1 <- lm(mpg~.-name+year*origin, data=Auto)
fit.int2 <- lm(mpg~.-name+year*weight, data=Auto)
fit.int3 <- lm(mpg~.-name+displacement*weight, data=Auto)

summary(fit.int1)
summary(fit.int2)
summary(fit.int3)

##(f)
lm_tran <- lm(mpg~.-name-horsepower+sqrt(horsepower),
              data = Auto)
lm_tran2 <- lm(mpg~.-name-weight+I(log(weight)),
              data = Auto)
lm_tran3 <- lm(mpg~.-name-weight+I((weight)^2),
              data = Auto)

summary(lm_tran)
summary(lm_tran2)
summary(lm_tran3)

#Exercise 3.13
set.seed(1)
##(a)
x<- rnorm(100)
##(b)
eps <- rnorm(100,0,0.5)
##(c)
y <- -1+0.5*x+eps
length(y)

##(d)
plot(x,y)
##(e)
lm1 <- lm(y~x)

##(f)
plot(x,y)
abline(-1,0.5, col = 'red')
abline(lm1, col = 'blue')
legend(0.5,-2,legend=c('population','linear model'),
       col = c('red','blue'),lwd = 2)

##(g)
lm1.2 <- lm(y~x+I(x^2))
summary(lm1.2)
anova(lm1, lm1.2)

##(h)
#eps2 <- rnorm(100, sd=0.3)
eps2 <- rnorm(100, sd=0.1)
y2 <- -1 + 0.5*x + eps2
lm2 <- lm(y2 ~ x)
summary(lm2)
plot(x,y2, main = 'sd = 0.1')
abline(-1,0.5, col = 'red')
abline(lm2, col = 'blue')
legend(0.7,-1.7,legend=c('population','linear model'),
       col = c('red','blue'),lwd = 2)

##(i)
#eps3 <- rnorm(100, sd=0.8)
eps3 <- rnorm(100, sd=1)
y3 <- -1 + 0.5*x + eps3
lm3 <- lm(y3 ~ x)
summary(lm3)
plot(x,y3, main = 'sd = 1')
abline(-1,0.5, col = 'red')
abline(lm3, col = 'blue')
legend(0.7,-3,legend=c('population','linear model'),
       col = c('red','blue'),lwd = 2)

##(j)
confint(lm1)
confint(lm2)
confint(lm3)

#Exercise 3.14
##(a)
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1+rnorm (100)/10
y <- 2+2*x1 +0.3*x2+rnorm (100)
lm1<-lm(y~x1+x2)
lm1

##(b)
cor(x1,x2)
plot(x1, x2)

##(c)
summary(lm1)

##(d)
lm2 <- lm(y~x1)
summary(lm2)
##(e)
lm3 <- lm(y~x2)
summary(lm3)

##(f)

##(g)
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y,6)

lm1.g <- lm(y~x1+x2)
summary(lm1.g)
plot(x1,x2)
lm1.g2 <- lm(y~x2)
summary(lm1.g2)
plot(x2,y)
points(0.8,6, col = 'red', pch = 4)
