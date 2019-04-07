setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Datamining/lab")

###1.The stock Market Data###
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[,-9]) #must be numeric
attach(Smarket)
plot(Volume)
plot(Year,Volume) 

###2.Logistic Regression###
lr.fit <- glm(Direction~.-Year-Today, data = Smarket, family = binomial)
summary(lr.fit)

coef(lr.fit)
summary(lr.fit)$coef
summary(lr.fit)$coef[,4] #only p-value

lr.probs = predict(lr.fit, type = 'response')
lr.probs[1:10]

contrasts(Direction)

lr.pred <- rep('Down', 1250)
lr.pred[lr.probs > 0.5] <- 'Up'

table(lr.pred, Direction)
#accuracy
(507+145)/1250
##or
mean(lr.pred == Direction)

#train, test split
train <- (Year < 2005)
Smarket.2005 <- Smarket[!train,] #X_test
dim(Smarket.2005)
Direction.2005 <- Direction[!train] #y_test

#fitting train data
lr.fit <- glm(Direction~.-Year-Today, family = binomial,
              data = Smarket, subset = train) #using subset
#predict test data
lr.probs <- predict(lr.fit, Smarket.2005, type = 'response')
lr.pred <- rep('Down', 252)
lr.pred[lr.probs > 0.5] <- 'Up'

table(lr.pred, Direction.2005)
#accuracy
mean(lr.pred == Direction.2005)

lr.fit <- glm(Direction~Lag1 + Lag2, family = binomial,
              data = Smarket, subset = train)
lr.probs <- predict(lr.fit, Smarket.2005, type = 'response')
lr.pred <- rep('Down', 252)
lr.pred[lr.probs > 0.5] <- 'Up'
table(lr.pred, Direction.2005)
#accuracy
mean(lr.pred == Direction.2005)


###3.LDA###
library(MASS)
lda.fit <- lda(Direction~Lag1+Lag2, data = Smarket, subset = train)
lda.fit

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posterior[,1] < 0.5)

lda.pred$posterior[1:20, 1]
lda.class[1:20]

sum(lda.pred$posterior[,1]>0.9)


###4.QDA###
qda.fit <- qda(Direction~Lag1+Lag2, data = Smarket, subset = train)
qda.fit

qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

###5.KNN###
library(class) #for knn

train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

set.seed(1)
#k=1
knn.pred<-knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred== Direction.2005)

#k=3
knn.pred <-knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred== Direction.2005)

###6.An Application to Caravan Insurance Data###

dim(Caravan)
attach(Caravan)
summary(Caravan)

standardized.X <- scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])

var(standardized.X[,1])
var(standardized.X[,2])

#train, test split
test<-1:1000

train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]

###knn###
set.seed(1)
##k=1
knn.pred <- knn(train.X, test.X, train.Y, k=1)
#accuracy
mean(test.Y == knn.pred)

mean(test.Y!=knn.pred)
mean(test.Y!=" No")

table(knn.pred, test.Y)

##k=3
knn.pred <- knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
##K=5
knn.pred <- knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)


###logistic Regression###
lr.fit <- glm(Purchase~., data = Caravan, family = binomial,
              subset = -test)

lr.probs <- predict(lr.fit, Caravan[test,], type = 'response')

#threshold = 0.5
lr.pred <- rep('No', 1000)
lr.pred[lr.probs>0.5] <- 'Yes'
table(lr.pred, test.Y)

#threshold = 0.25
lr.pred <- rep('No', 1000)
lr.pred[lr.probs>0.25] <- 'Yes'
table(lr.pred, test.Y)