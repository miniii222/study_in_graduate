setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Datamining/hw")

###Exercise2
#(d)
n <- 5;1-(1-1/n)^n
#(e)
n<-100;1-(1-1/n)^n
#(f)
n<-10000;1-(1-1/n)^n
#(g)
n<- 1:100000
plot(n, 1-(1-1/n)^n)
#(h)
store <- rep(NA, 10000)
for(i in 1:10000){
  store[i] <- sum(sample(100, rep = T) == 4) > 0
}

mean(store)
###Exercise5
library(ISLR); data(Default)
set.seed(1994)

#(a)
lr1 <- glm(default~income+balance, data = Default, family = 'binomial')
summary(lr1)
#(b)

##1.split train, validation set 7:3
train.ind <- sample(nrow(Default),as.integer(nrow(Default)*0.7))
train <- Default[train.ind, ]
valid  <- Default[-train.ind,]
##2.fit a multiple logistic regression
lr1 <- glm(default~income+balance, data = train, family = 'binomial')
##3.obtain a prediction
lr1.prob <- predict(lr1, valid, 'response')
lr1.pred <- ifelse(lr1.prob >=0.5, 'Yes',"No" )
##4.Compute the validation set error
table(lr1.pred, valid$default)
mean(lr1.pred != valid$default)

#(c)

lr.err <- c()
for(i in 1:3){
  
  train.ind <- sample(nrow(Default),as.integer(nrow(Default)*0.7))
  train <- Default[train.ind, ]
  valid  <- Default[-train.ind,]
  
  lr1 <- glm(default~income+balance, data = train, family = 'binomial')

  lr1.prob <- predict(lr1, valid, 'response')
  lr1.pred <- ifelse(lr1.prob >=0.5, 'Yes',"No" )
  
  lr.err[i] <- mean(lr1.pred != valid$default)
}
lr.err

#(d)
Default$stu_dummy <- ifelse(Default$student=='Yes',1,0)
lr.err <- c()
for(i in 1:3){
  
  train.ind <- sample(nrow(Default),as.integer(nrow(Default)*0.7))
  train <- Default[train.ind, ]
  valid  <- Default[-train.ind,]
  
  lr1 <- glm(default~income+balance+stu_dummy, data = train, family = 'binomial')
  
  lr1.prob <- predict(lr1, valid, 'response')
  lr1.pred <- ifelse(lr1.prob >=0.5, 'Yes',"No" )
  
  lr.err[i] <- mean(lr1.pred != valid$default)
}
lr.err

###Exercise7
data(Weekly)
#(a)
lr1 <- glm(Direction~Lag1+Lag2, family = 'binomial', data = Weekly)
summary(lr1)

#(b)
lr1_1 <- glm(Direction~Lag1+Lag2, family = 'binomial', data = Weekly[-1,])
summary(lr1_1)
#(c)
lr1_1.prob <- predict(lr1_1, Weekly[1,], 'response')
lr1_1.pred <- ifelse(lr1_1.prob > 0.5, 'Up', 'Down')
#not correct
lr1_1.pred ==  Weekly[1,'Direction']

#(d)
n <- nrow(Weekly)
lr1_i.err <-c()
for(i in 1:n){
  
  #1
  lr1_i <- glm(Direction~Lag1+Lag2, family = 'binomial', data = Weekly[-i,])
  #2
  lr1_i.prob <- predict(lr1_i, Weekly[i,], 'response')
  #3
  lr1_i.pred <- ifelse(lr1_i.prob>0.5, 'Up','Down')
  #4
  lr1_i.err[i] <- ifelse(lr1_i.pred == Weekly[i,'Direction'],0,1)
  
}

lr1_i.err
sum(lr1_i.err)
#(e)
mean(lr1_i.err)

###Exercise9
library(MASS); data(Boston)
#(a)
mean(Boston$medv)
#(b)
n <- nrow(Boston)
sd(Boston$medv) / sqrt(n)
#(c)
BS <- 100000
bs.mean <- c()
for (i in 1:BS){
  
  bs.ind <- sample(n, replace = T)
  bs.mean[i] <- mean(Boston$medv[bs.ind])
  
}

sd(bs.mean)

#(d)
t.test(Boston$medv)

sort(bs.mean)[c(0.025*BS+1, 0.975*BS-1)]

#(e)
median(Boston$medv)
#(f)
bs.median <- c()
for (i in 1:BS){
  
  bs.ind <- sample(n, replace = T)
  bs.median[i] <- median(Boston$medv[bs.ind])
}

sd(bs.median)
#(g)
quantile((Boston$medv), 0.1)

#(h)
bs.10 <- c()
for(i in 1:BS){
  bs.ind <- sample(n, replace = T)
  bs.10[i] <- quantile(Boston$medv[bs.ind], 0.1)
}

sd(bs.10)
