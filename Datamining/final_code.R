Caravan <- read.csv("Caravan.csv")

#(a)
dim(Caravan)
table(Caravan$Purchase)

Caravan$Purchase <- ifelse(Caravan$Purchase == 'Yes',1,0)

train <- Caravan[1:1000,]
test <- Caravan[1001:5822,]

#(b)
##GBM
library(gbm)
gbm1 <- gbm(Purchase~., data = train, n.trees = 1000, shrinkage = 0.01)
summary(gbm1)

##XGBoost
library(xgboost)
X_train <- as.matrix(train[,-86]); y_train <- train$Purchase
X_test <- as.matrix(test[,-86]); y_test <- test$Purchase


xgb1 <- xgboost(data = X_train, label = y_train, nrounds = 10,
                objective = "binary:logistic")

# Compute feature importance matrix
importance_matrix <- xgb.importance(colnames(X_train), model = xgb1)
# Nice graph
xgb.plot.importance(importance_matrix[1:50,])
importance_matrix


#(C)
##gbm
gbm_pred <-  predict(gbm1, test, n.trees = 1000, type = "response")
gbm_pred <- ifelse(gbm_pred > 0.2 , 1, 0)
table(gbm_pred, test$Purchase)

##xgboost
xgb_pred <- predict(xgb1, X_test)
xgb_pred <- ifelse(xgb_pred > 0.2 , 1, 0)
table(xgb_pred, test$Purchase)

##logistic regression
lr <- glm(Purchase~., data = train, family = binomial)
lr_pred <- predict(lr, test, 'response')
lr_pred <- ifelse(lr_pred > 0.2 , 1, 0)
table(lr_pred, test$Purchase)

##randomforest
library(randomForest)
rf1 <- randomForest(Purchase~., data = train)
rf_pred <- predict(rf1, test)
rf_pred <- ifelse(rf_pred > 0.2 , 1, 0)
table(rf_pred, test$Purchase)

##knn
library(class)
knn1 <- knn(X_train, X_test,y_train)
table(knn1, y_test)

##total plot
total <- data.frame(model = c('gbm','xgboost','knn',
                              'logistic','randomforest'),
                    value = c(0.2133,0.1214,0.087,0.1421,0.1391))

library(ggplot2)
ggplot(total)+geom_bar(aes(model, value, fill = model),
                       stat = 'identity') +
      scale_y_continuous(labels = scales::percent)