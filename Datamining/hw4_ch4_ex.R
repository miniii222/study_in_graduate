library(ISLR)

#Exercise.10
data(Weekly)

#(a)
summary(Weekly)
plot(Weekly)

#(b)
lr1 <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
           data = Weekly, family = binomial)
summary(lr1)

#(c)
lr.proba <- predict(lr1, Weekly, 'response')
contrasts(Weekly$Direction)
lr.pred <- ifelse(lr.proba >=0.5, 'Up','Down')

table(lr.pred, Weekly$Direction)
mean(lr.pred == Weekly$Direction)

#(d)
train <- Weekly[Weekly$Year<2009, ]
test <- Weekly[Weekly$Year>=2009, ]

lr2 <- glm(Direction~Lag2, data = train, family = binomial)
lr2.proba <-predict(lr2, test, 'response')
lr2.pred <- ifelse(lr2.proba >=0.5, "Up",'Down')

table(lr2.pred, test$Direction)
mean(lr2.pred == test$Direction)

#(e)
library(MASS)
lda1 <- lda(Direction~Lag2, data = train)
lda1.pred <- predict(lda1, test)

table(lda1.pred$class, test$Direction)
mean(lda1.pred$class == test$Direction)

#(f)
qda1 <- lda(Direction~Lag2, data = train)
qda1.pred <- predict(qda1, test)

table(qda1.pred$class, test$Direction)
mean(qda1.pred$class == test$Direction)

#(g)
library(class)
X_train <- as.matrix(train$Lag2)
X_test <- as.matrix(test$Lag2)
y_train <- train$Direction

knn1 <- knn(X_train,X_test, y_train, k=1)
table(knn1, test$Direction)
mean(knn1 == test$Direction)

#(h)
##knn is worst

#(i)
#logistic regression
lr1 <- glm(Direction~.-Year, data = train, family = binomial)
lr1.proba <- predict(lr1, test, 'response')
lr1.pred <- ifelse(lr1.proba >= 0.5,'Up','Down')
table(lr1.pred, test$Direction) #accuracy : 1
mean(lr1.pred == test$Direction)

#lda
lda1 <- lda(Direction~.-Year, data = train)
lda1.pred <- predict(lda1, test)
table(lda1.pred$class, test$Direction) #accuracy : 1
mean(lda1.pred$class == test$Direction)

#qda
qda1 <- qda(Direction~.-Year, data = train)
qda1.pred <- predict(qda1, test)
table(qda1.pred$class, test$Direction) #accuracy : 1
mean(qda1.pred$class == test$Direction)

#knn
X_train <- train[,-9]
X_test <- test[,-9]

k_list <- seq(10,100, 10)
knn_acc <- rep(0, length(k_list))

for(i in 1:length(k_list)){
  knn1 <- knn(X_train, X_test, y_train, k = i)
  knn_acc[i] <- mean(knn1==test$Direction)
}
knn_acc
plot(k_list, knn_acc)

k_list <- 25:40
knn_acc <- rep(0, length(k_list))

for(i in 1:length(k_list)){
  knn1 <- knn(X_train, X_test, y_train, k = i)
  knn_acc[i] <- mean(knn1==test$Direction)
}
knn_acc
plot(k_list, knn_acc)

k = 35
knn.best <- knn(X_train, X_test, y_train ,k)
table(knn.best, test$Direction)
mean(knn.best == test$Direction)


#Excercise.11
#(a)
data(Auto)
mpg01 <- ifelse(Auto$mpg > median(Auto$mpg),1,0)
dt <- Auto
dt$mpg01 <- mpg01
dim(dt)

#(b)
par(mfrow = c(2,2))
plot(Auto$cylinders,mpg01)
plot(Auto$displacement,mpg01)
plot(Auto$horsepower,mpg01)
plot(Auto$weight,mpg01)
plot(Auto$acceleration,mpg01)
plot(Auto$year,mpg01)
plot(Auto$origin,mpg01)

library(ggplot2);library(gridExtra)
p1 <- ggplot(dt)+geom_boxplot(aes(mpg01, cylinders, group = mpg01))
p2 <- ggplot(dt)+geom_boxplot(aes(mpg01, displacement, group = mpg01))
p3 <- ggplot(dt)+geom_boxplot(aes(mpg01, horsepower, group = mpg01))
p4 <- ggplot(dt)+geom_boxplot(aes(mpg01, weight, group = mpg01))
grid.arrange(p1,p2,p3,p4, nrow=2)

p1 <- ggplot(dt)+geom_boxplot(aes(mpg01, acceleration, group = mpg01))
p2 <- ggplot(dt)+geom_boxplot(aes(mpg01, year, group = mpg01))
p3 <- ggplot(dt)+geom_boxplot(aes(mpg01, origin, group = mpg01))
grid.arrange(p1,p2,p3, nrow=2)

#displacement, horsepower,weight,acceleration

#(c)
#split train, test data (7:3)
train_ind <- sample(nrow(dt),as.integer(nrow(dt)*0.7))
train <- dt[train_ind, ]
test <- dt[-train_ind, ]

#(d)
library(MASS)
lda1 <- lda(mpg01~displacement + horsepower+weight+acceleration,
            data = dt)
lda.pred <- predict(lda1, test)
table(lda.pred$class, test$mpg01)
mean(lda.pred$class != test$mpg01)

#(e)
qda1 <- qda(mpg01~displacement + horsepower+weight+acceleration,
            data = dt)
qda.pred <- predict(qda1, test)
table(qda.pred$class, test$mpg01)
mean(qda.pred$class != test$mpg01)

#(f)
lr1 <- glm(mpg01~displacement + horsepower+weight+acceleration,
           data = dt, family = binomial)
lr.proba <- predict(lr1, test, 'response')
lr.pred <- ifelse(lr.proba >= 0.5,1,0)
table(lr.pred, test$mpg01)
mean(lr.pred != test$mpg01)

#(g)
library(class)
train.X <- train[,c('displacement','horsepower','weight','acceleration')]
test.X <- test[,c('displacement','horsepower','weight','acceleration')]
train.y <- train$mpg01


k_list <- seq(10,100, by = 10)
knn_miss <- rep(0,length(k_list))

for(i in 1:length(k_list)){
  knn1 <- knn(train.X, test.X, train.y, k = k_list[i])
  knn_miss[i] <- mean(knn1 != test$mpg01)
}
plot(k_list, knn_miss)

k_list <- 1:15
knn_miss <- rep(0,length(k_list))
plot(k_list, knn_miss)

for(i in 1:length(k_list)){
  knn1 <- knn(train.X, test.X, train.y, k = k_list[i])
  knn_miss[i] <- mean(knn1 != test$mpg01)
}
plot(k_list, knn_miss)

k=5
knn.best <- knn(train.X, test.X, train.y ,k)
table(knn.best, test$mpg01)
mean(knn.best != test$mpg01)
