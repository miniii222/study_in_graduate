setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Datamining/hw")

#8
library(tree);library(ISLR)

attach(Carseats)
High <- ifelse(Sales <= 8,'No','Yes')
Carseats <- data.frame(Carseats, High)
dim(Carseats)

##(a)
#train,test 7:3
set.seed(1)
train.ind <- sample(nrow(Carseats)*0.7)
train <- Carseats[train.ind,]
test <- Carseats[-train.ind,]

dim(train); dim(test)

##(b)
tree1 <- tree(High~. - Sales, data = train)
plot(tree1); text(tree1,pretty = 0)
tree1.pred <- predict(tree1, test, 'class')
table(tree1.pred, test$High)

mean(tree1.pred != test$High)

#(c)
tree1.cv <- cv.tree(tree1)
tree1.cv
plot(tree1.cv$size, tree1.cv$dev)
points(tree1.cv$size[which.min(tree1.cv$dev)],min(tree1.cv$dev),
       col = "red", cex = 2, pch = 20)

tree1.prune <- prune.tree(tree1, best = 5)
tree1.pred <- predict(tree1.prune, test, 'class')

table(tree1.pred, test$High)
mean(tree1.pred != test$High)

#(d),(e)
library(randomForest)

rf1 <- randomForest(High~.-Sales, data = train, importance = T)
rf1.pred <- predict(rf1, test)
table(rf1.pred, test$High)
mean(rf1.pred != test$High)

importance(rf1)
varImpPlot(rf1)

#9
data("OJ")
dim(OJ)

#(a)
set.seed(1)
train.ind <- sample(nrow(OJ), 800)
OJ.train <- OJ[train.ind,]
OJ.test <- OJ[- train.ind,]

#(b)
tree2 <- tree(Purchase ~., data = OJ.train)
summary(tree2)

#(c)
tree2

#(d)
plot(tree2);text(tree2)

#(e)
tree2.pred <- predict(tree2, OJ.test, 'class')
table(tree2.pred, OJ.test$Purchase)
mean(ifelse(tree2.pred == OJ.test$Purchase,0,1)) #0.1703704

#(f)
tree2.cv <- cv.tree(tree2)

#(g)
plot(tree2.cv$size, tree2.cv$dev)
points(tree2.cv$size[which.min(tree2.cv$dev)],min(tree2.cv$dev),
       col = "red", cex = 2, pch = 20)
#(h)
tree2.cv$size[which.min(tree2.cv$dev)]

#(i)
tree2.prune <- prune.tree(tree2, best = 9)

#(j)
summary(tree2.prune)

#(k)
tree2.pred <- predict(tree2.prune, OJ.test, 'class')
table(tree2.pred, OJ.test$Purchase)
mean(ifelse(tree2.pred == OJ.test$Purchase,0,1))


#10
data("Hitters")

#(a)
colnames(Hitters)
colSums(is.na(Hitters))

#remove NAs
Hitters <- Hitters[complete.cases(Hitters), ]
Hitters$Salary <- log(Hitters$Salary)

hist(Hitters$Salary, breaks = 30)
dim(Hitters)

#(b)
Hit.train <- Hitters[1:200, ]
Hit.test <- Hitters[200:nrow(Hitters),]

#(c)
mse_fun <- function(pred, true){
  mean((pred-true)^2)
}

library(gbm)
lambda <- 10^seq(-5, -0.4, by = 0.05)

mse_df <- data.frame(lambda = lambda,
              train_mse = NA,test_mse = NA)

for(i in 1:length(lambda)){
  gbm1 <- gbm(Salary~., data = Hit.train,
            distribution = 'gaussian', n.trees = 1000,
            shrinkage = lambda[i])

  gbm1.train.pred <- predict(gbm1, Hit.train, n.trees = 1000)
  mse_df$train_mse[i] = mse_fun(Hit.train$Salary,gbm1.train.pred)
  
}

plot(mse_df$lambda, mse_df$train_mse, type = 'b')

#(d)
for(i in 1:length(lambda)){
  gbm1 <- gbm(Salary~., data = Hit.train,
              distribution = 'gaussian', n.trees = 1000,
              shrinkage = lambda[i])
  
  gbm1.test.pred <- predict(gbm1, Hit.test, n.trees = 1000)
  mse_df$test_mse[i] = mse_fun(Hit.test$Salary, gbm1.test.pred)
  
}

plot(mse_df$lambda, mse_df$test_mse, type = 'b')
points(mse_df$lambda[which.min(mse_df$test_mse)],min(mse_df$test_mse),
       col = "red", cex = 2, pch = 20)
mse_df[which.min(mse_df$test_mse),]

#(e)
###linear regression
lr1 <- lm(Salary~., data = Hit.train)
mse_fun(predict(lr1, Hit.test), Hit.test$Salary)


library(glmnet)
X.train <- model.matrix(Salary ~., data = Hit.train)
X.test <- model.matrix(Salary ~., data = Hit.test)
y.train <- Hit.train$Salary
y.test <- Hit.test$Salary

###lasso
mse_df$lasso <- NA
lasso1 <- glmnet(X.train, y.train, alpha = 1)
for(i in 1:length(lambda)){
  
  lasso1.pred <- predict.glmnet(lasso1, newx = X.test,
                                s = lambda[i])
  mse_df$lasso[i] <- mse_fun(lasso1.pred, Hit.test$Salary)
}

plot(mse_df$lambda, mse_df$lasso, type = 'b',
     main = 'Lasso')
points(mse_df$lambda[which.min(mse_df$lasso)],min(mse_df$lasso),
       col = "red", cex = 2, pch = 20)

###ridge
mse_df$ridge <- NA
ridge1 <- glmnet(X.train, y.train, alpha = 0)

for(i in 1:length(lambda)){
  
  ridge1.pred <- predict.glmnet(ridge1, newx = X.test,
                                s = lambda[i])
  mse_df$ridge[i] <- mse_fun(ridge1.pred, Hit.test$Salary)
}

plot(mse_df$lambda, mse_df$ridge, type = 'b',
     main = 'Ridge')
points(mse_df$lambda[which.min(mse_df$ridge)],min(mse_df$ridge),
       col = "red", cex = 2, pch = 20)

#(f)
mse_df[which.min(mse_df$test_mse),]

gbm.best <- gbm(Salary~., data = Hit.train, distribution = 'gaussian',
                n.trees = 1000, shrinkage = 0.08912509)
summary(gbm.best)


#(g)
rf1 <- randomForest(Salary~., data = Hit.train)
rf1.pred <- predict(rf1, Hit.test)
mse_fun(rf1.pred, Hit.test$Salary)
varImpPlot(rf1)
