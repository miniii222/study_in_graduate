setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Datamining")
#https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/

library(data.table)
library(mlr);library(readr)

setcol<- c("age","workclass","fnlwgt","education","education-num",
           "marital-status","occupation","relationship","race",
           "sex","capital-gain","capital-loss","hours-per-week",
           "native-country","target")

train <- read_csv("adult_data.txt", col_names = setcol)
test <- read_csv("adult_test.txt", col_names = setcol)

setDT(train); setDT(test)

#check missing values
colSums(train[,is.na(train)]) #NA 0
colSums(test[,is.na(test)]) #NA 14

#quick data cleaning
library(stringr)
##remove extra character from target variable
test[,target := substr(target, 1, nchar(target)-1)]

##remove leading whitespaces
sapply(test, is.character)
char_col <- colnames(train)[sapply(test, is.character)]
for(i in char_col) set(train,j=i,value = str_trim(train[[i]],side = "left"))
for(i in char_col) set(test,j=i,value = str_trim(test[[i]],side = "left"))

train[is.na(train)] <- "Missing" 
test[is.na(test)] <- "Missing" 

#using one hot encoding 
labels <- train$target 
ts_label <- test$target
new_tr <- model.matrix(~.+0,data = train[,-c("target"),with=F]) 
new_ts <- model.matrix(~.+0,data = test[,-c("target"),with=F])

#convert factor to numeric
table(labels)
table(ts_label)
labels <- ifelse(labels == '<=50K',1,0)
ts_label <- ifelse(ts_label == '<=50K',1,0)

library(xgboost)
dtrain <- xgb.DMatrix(data = new_tr, label = labels)
dtest <- xgb.DMatrix(data = new_ts, label = ts_label)

params <- list(booster = 'gbtree', objective = 'binary:logistic',
               eta = 0.3, gamma = 0,
               max_depth = 6, min_child_weight = 1,
               subsample = 1, colsample_bytree = 1)

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5,
                 showsd = T, stratified = T,
                 print_every_n= 10, early_stop_round = 20, maximize = F)

names(xgbcv)
xgbcv$evaluation_log[,'test_error_mean']
min(xgbcv$evaluation_log[,'test_error_mean']) #best_iteration = 75

xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 75,
                   watchlist = list(val = dtest, train = dtrain),
                   print_every_n = 10, early_stopping_rounds = 10,
                   maximize = F, eval_metric = 'error')
xgbpred <- predict(xgb1, dtest)
