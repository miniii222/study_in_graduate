setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Datamining/lab")
#https://cran.r-project.org/web/packages/xgboost/vignettes/xgboostPresentation.html

require(xgboost)
data("agaricus.train", package = 'xgboost')
data("agaricus.test", package = 'xgboost')

train <- agaricus.train; test <- agaricus.test

str(train)
dim(train$data);dim(test$data)

bstSparse <- xgboost(data = train$data, label = train$label,
                     max_depth = 2, eta = 1,
                     nthread = 2, nrounds = 2, #nthread : core / nrounds : iteration
                     objective = 'binary:logistic')


dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1,
               nrounds = 2, objecive = 'binary:logistic', verbose = 1)
pred <- predict(bst, test$data)
head(pred)

prediction <- as.numeric(pred >0.5)
head(prediction)

err <- mean(prediction != test$label)
print(paste("test-error=", err))

dtrain <- xgb.DMatrix(data = train$data, label = train$label)
dtest <- xgb.DMatrix(data = test$data, label = test$label)

watchlist <- list(train = dtrain, test = dtest)
bst <- xgb.train(data = dtrain, max_depth = 2, eta = 1, nrounds = 2,
                 watchlist = watchlist, objective = 'binary:logistic')
bst <- xgb.train(data = dtrain, max_depth = 2, eta = 1, nrounds = 2,
                 watchlist = watchlist, objective = 'binary:logistic',
                 eval_metric = 'error', eval_metric = 'logloss')
#eval_metric 두 개 걸면 두 개 출력


###lienar boosting
bst <- xgb.train(data = dtrain, max_depth = 2, eta = 1, nrounds = 2,
                 watchlist = watchlist, objective = 'binary:logistic',
                 eval_metric = 'error', eval_metric = 'logloss',
                 booster = 'gblinear')
#In this specific case, linear boosting gets sligtly better performance metrics
#than decision trees based algorithm

###feature importance
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


xgb.dump(bst, with_stats = T)