library(readr)
baseball <- read_table2("baseball.txt")

lm1<-lm(salary~., data = baseball)
lm1.null<-lm(salary~1., data = baseball)

#backward AIC 4417.59
lm1.back<-step(lm1) 
summary(lm1.back)
#forward AIC 4419.51
lm1.for<-step(lm1.null, direction = 'forward',scope=formula(lm1))
#both AIC 4417.59
lm1.both<-step(lm1, direction = 'both')

library(leaps)
reg1<-regsubsets(salary~., data = baseball, nvmax=28)
plot(reg1)
reg1.sum<-summary(reg1)
X11()

plot(reg1, scale = "Cp")
plot(reg1.sum$cp, xlab = "Number of Variables", ylab = "Cp")
points(8, reg1.sum$cp[8], pch = 20, col = "red")

plot(reg1.sum$rsq, xlab = "Number of Variables", ylab = "r-squared")
points(8, reg1.sum$rsq[8], pch = 20, col = "red")

lm.reg<-lm(salary~., data=baseball[,reg1.sum$which[8,]])
step(lm.reg)
coef(reg1,8)
