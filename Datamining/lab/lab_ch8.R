setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Datamining/lab")

###1.Fitting Classification Trees###
library(tree);library(ISLR)

attach(Carseats)
High <- ifelse(Sales <= 8,'No','Yes')
Carseats <- data.frame(Carseats, High)

tree.carseats <- tree(High~.-Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats);text(tree.carseats, pretty = 0)
#most important indicator : ShelveLoc

tree.carseats

set.seed(2)
train <- sample(nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]

tree.carseats <- tree(High~. -Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = 'class')

table(tree.pred, High.test)
#accuracy
(104+50)/200

#pruning
set.seed(2)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
#dev : cross-validation error rate
#tree with 5 terminal nodes results in the lowest cv error rate
cv.carseats

par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')

prune.carseats <- prune.misclass(tree.carseats, best = 5)
par(mfrow = c(1,1))
plot(prune.carseats);text(prune.carseats, pretty = 0)

tree.pred <- predict(prune.carseats, Carseats.test, type = 'class')
table(tree.pred, High.test)

prune.carseats <- prune.misclass(tree.carseats, best = 15)
plot(prune.carseats);text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test, type = 'class')
table(tree.pred, High.test)

###2.Fitting Regression Trees###
library(MASS)
set.seed(1)

train <- sample(nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv~., Boston, subset = train)
summary(tree.boston)

plot(tree.boston);text(tree.boston, pretty = 0)

#cv.tree랑 cv.misclas랑 거의 동일
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')

prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston);text(prune.boston)

yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train,'medv']
plot(yhat, boston.test)
abline(0,1)
#mse
mean((yhat - boston.test)^2)


###3.Bagging and RandomForest###
library(randomForest)
set.seed(1)

bag.boston <- randomForest(medv~., data = Boston, subset = train,
                           mtry = 13, importance = T)
#mtry = 13 : all 13 predictors should be considered for each split of the tree
bag.boston

yhat.bag <- predict(bag.boston, Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
#mse
mean((yhat.bag - boston.test)^2)

bag.boston <- randomForest(medv~., data = Boston, subset = train,
                           mtry = 13, ntree = 25)
yhat.bag <- predict(bag.boston, Boston[-train,])
mean((yhat.bag - boston.test)^2)

set.seed(1)
rf.boston <- randomForest(medv~., data = Boston, subset = train,
                          mtry = 6, importance = T)
yhat.rf <- predict(rf.boston, Boston[-train,])

mean((yhat.rf - boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)

###4.Boosting###
library(gbm)
set.seed(1)

boost.boston <- gbm(medv~., dat = Boston[train,],distribution = 'gaussian',
                    n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

par(mfrow = c(1,2))
plot(boost.boston, i = 'rm')
plot(boost.boston, i = 'lstat')

yhat.boost <- predict(boost.boston, Boston[-train,],
                      n.trees = 5000)
mean((yhat.boost - boston.test)^2)

#shrinkage
boost.boston <- gbm(medv~., data = Boston[train,],
                    distribution = 'gaussian',n.trees = 5000,
                    interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost <- predict(boost.boston, Boston[-train,],
                      n.trees = 5000)
mean((yhat.boost - boston.test)^2)
