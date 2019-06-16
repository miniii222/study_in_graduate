setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Advanced_linear_models/papers/missing")
library(readr)
customers <- read_csv("C:/Users/wjssm/Desktop/0.graduate/3rd/Advanced_linear_models/papers/data/Wholesale_customers_data.csv")
customers$Channel <- ifelse(customers$Channel == 2 ,0,1)
summary(customers)
options(warn=-1) #options(warn = 0)
# 1) FRESH: annual spending (m.u.) on fresh products (Continuous); 
# 2) MILK: annual spending (m.u.) on milk products (Continuous); 
# 3) GROCERY: annual spending (m.u.)on grocery products (Continuous); 
# 4) FROZEN: annual spending (m.u.)on frozen products (Continuous) 
# 5) DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous) 
# 6) DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous); 
# 7) CHANNEL: customersâ€™ Channel - Horeca (Hotel/Restaurant/CafÃ©) or Retail channel (Nominal) 
###### 1 : Horeca 298(67.73%) / 0 : Retail 142(32.27%)
# 8) REGION: customersâ€™ Region â€“ Lisnon, Oporto or Other (Nominal)
###### 1 : Lisbon 77 / 2 : Oporto 47 / 3 : Other Region 316 

dim(customers) #440   8

n <- nrow(customers)

# train test split

# library(caret)
# intrain <- createDataPartition(y=customers$Channel, p=0.7, list=FALSE)
# train <- customers[intrain, ] ; test <- customers[-intrain, ]
# table(train$Channel); table(test$Channel)
# 
# write.csv(train, file = 'train.csv', row.names = F)
# write.csv(test, file = 'test.csv', row.names = F)


train <- read.csv('train.csv')
test <- read.csv('test.csv')


# baseline data
lr <- glm(Channel ~ . , data = train, family = binomial)
pred <- predict(lr, test, 'response')
pred <- ifelse(pred > 0.5, 1, 0)
#misclassification rate : 10.6060606%
mean(ifelse(pred == test$Channel, 0,1))


#########################
#####     MCAR     ######
#########################

set.seed(0610)
library(missForest)
col <- c('Fresh','Milk','Grocery','Frozen')

df_MCAR <- prodNA(customers[col], 0.35) #전체 데이터의 20%
sum(is.na(df_MCAR)) #528

train_MCAR <- train; test_MCAR <- test;
train_MCAR[col] <- df_MCAR[intrain,col]
test_MCAR[col] <- df_MCAR[-intrain,col]

n_train_miss <- sum(is.na(train_MCAR)) #429
n_test_miss <- sum(is.na(test_MCAR)) #187
n_total_miss <- n_train_miss + n_test_miss #616
# write.csv(train_MCAR, 'train_MCAR.csv', row.names = F)
# write.csv(test_MCAR, 'test_MCAR.csv', row.names = F)


train_MCAR <- read.csv('train_MCAR.csv')
test_MCAR <- read.csv('test_MCAR.csv')
df_MCAR <- rbind(train_MCAR, test_MCAR)


#1.mean Imputation
train_mean <- data.frame(train_MCAR)
test_mean <- data.frame(test_MCAR)
df_mean <- data.frame(df_MCAR)

for(i in 3:6) {
  train_mean[ , i][is.na(train_mean[ , i])] <- mean(train_mean[ , i], na.rm = TRUE)
  test_mean[ , i][is.na(test_mean[ , i])] <- mean(test_mean[ , i], na.rm = TRUE)
  df_mean[ , i][is.na(df_mean[ , i])] <- mean(df_mean[ , i], na.rm = TRUE)
}

split_impute <- function(train_impute, test_impute){
  
  #train, test : after imputing separately
  lr <- glm(Channel ~ . , data = train_impute, family = binomial)
  pred <- predict(lr, test_impute, 'response')
  pred <- ifelse(pred > 0.5, 1, 0)
  
  #misclassification rate
  miss <- round(mean(ifelse(pred == test_impute$Channel, 0,1)) * 100, 2)
  cat('misclassification rate: ',miss, '%')
  cat('\n')
  df <- rbind(train_impute, test_impute)
  df_scaled <- data.frame(scale(df))
  total_scaled <- data.frame(scale(customers))
  
  cat('rmse : ', sqrt(sum((total_scaled - df_scaled)^2) / (n_total_miss)))
}
split_impute(train_mean, test_mean)


impute_split <- function(df_impute){
  #together imputation -> split
  train_impute <- df_impute[intrain,]; test_impute <- df_impute[-intrain,]
  
  lr <- glm(Channel ~ . , data = train_impute, family = binomial)
  pred <- predict(lr, test_impute, 'response')
  pred <- ifelse(pred > 0.5, 1, 0)
  
  #misclassification rate
  miss <- round(mean(ifelse(pred == test_impute$Channel, 0,1)) * 100, 2)
  cat('misclassification rate: ',miss, '%')
  cat('\n')
  
  df_scaled <- data.frame(scale(df_impute))
  total_scaled <- data.frame(scale(customers))
  
  cat('rmse : ', sqrt(sum((total_scaled - df_scaled)^2) / (n_total_miss)))
}

impute_split(df_mean)



#2.MICE
library(mice)
train_mice <- mice(train_MCAR[,-1],seed = 333); train_mice <- complete(train_mice)
test_mice <- mice(test_MCAR[,-1],seed = 333); test_mice <- complete(test_mice)
df_mice <- mice(df_MCAR[,-1],seed = 333); df_mice <- complete(df_mice)

train_mice <- cbind(train$Channel, train_mice)
colnames(train_mice)[1] <- 'Channel'
test_mice <- cbind(test$Channel, test_mice)
colnames(test_mice)[1] <- 'Channel'
df_mice <- cbind(customers$Channel, df_mice)
colnames(df_mice)[1] <- 'Channel'

split_impute(train_mice, test_mice)
impute_split(df_mice)



#3.KNN
library(DMwR)
train_knn <- knnImputation(train_MCAR[,-1])
test_knn <- knnImputation(test_MCAR[,-1])
df_knn <- knnImputation(df_MCAR[,-1])

train_knn <- cbind(train$Channel, train_knn)
colnames(train_knn)[1] <- 'Channel'
test_knn <- cbind(test$Channel, test_knn)
colnames(test_knn)[1] <- 'Channel'
df_knn <- cbind(customers$Channel, df_knn)
colnames(df_knn)[1] <- 'Channel'

split_impute(train_knn, test_knn)
impute_split(df_knn)

#4.missForest
library(missForest)
set.seed(333)
train_miss <- missForest(train_MCAR[,-1])$ximp
test_miss <- missForest(test_MCAR[,-1])$ximp
df_miss <- missForest(df_MCAR[,-1])$ximp

train_miss <- cbind(train$Channel, train_miss)
colnames(train_miss)[1] <- 'Channel'
test_miss <- cbind(test$Channel, test_miss)
colnames(test_miss)[1] <- 'Channel'
df_miss <- cbind(customers$Channel, df_miss)
colnames(df_miss)[1] <- 'Channel'

split_impute(train_miss, test_miss)
impute_split(df_miss)

#########################
#####     MNAR     ######
#########################

df_MNAR <- data.frame(customers)
col
a1 <-1; a2 <- 0

set.seed(0) #619


df_MNAR$Fresh <- ifelse(a1 * scale(customers$Fresh) +a2 * df_MNAR$Channel
                        + rnorm(n) > 0, NA, df_MNAR$Fresh)
df_MNAR$Milk <- ifelse(a1 * scale(customers$Milk) +a2 * df_MNAR$Channel
                        + rnorm(n,-0.3) > 0, NA, df_MNAR$Milk)
df_MNAR$Grocery <- ifelse(a1 * scale(customers$Grocery) +a2 * df_MNAR$Channel
                        + rnorm(n,-0.5) > 0, NA, df_MNAR$Grocery)
df_MNAR$Frozen <- ifelse(a1 * scale(customers$Frozen) +a2 * df_MNAR$Channel
                        + rnorm(n,-0.5) > 0, NA, df_MNAR$Frozen)
sum(is.na(df_MNAR))
colSums(is.na(df_MNAR))

train_MNAR <- df_MNAR[intrain,]; test_MNAR <- df_MNAR[-intrain,]

n_train_miss <- sum(is.na(train_MNAR)) #444
n_test_miss <- sum(is.na(test_MNAR)) #175
n_total_miss <- n_train_miss + n_test_miss #619
# write.csv(train_MNAR, 'train_MNAR.csv', row.names = F)
# write.csv(test_MNAR, 'test_MNAR.csv', row.names = F)


train_MNAR <- read.csv('train_MNAR.csv')
test_MNAR <- read.csv('test_MNAR.csv')
df_MNAR <- rbind(train_MNAR, test_MNAR)

#1.mean Imputation
train_mean <- data.frame(train_MNAR)
test_mean <- data.frame(test_MNAR)
df_mean <- data.frame(df_MNAR)

for(i in 3:6) {
  train_mean[ , i][is.na(train_mean[ , i])] <- mean(train_mean[ , i], na.rm = TRUE)
  test_mean[ , i][is.na(test_mean[ , i])] <- mean(test_mean[ , i], na.rm = TRUE)
  df_mean[ , i][is.na(df_mean[ , i])] <- mean(df_mean[ , i], na.rm = TRUE)
}

split_impute(train_mean, test_mean)
impute_split(df_mean)


#2.MICE
library(mice)
train_mice <- mice(train_MNAR[,-1],seed = 333); train_mice <- complete(train_mice)
test_mice <- mice(test_MNAR[,-1],seed = 333); test_mice <- complete(test_mice)
df_mice <- mice(df_MNAR[,-1],seed = 333); df_mice <- complete(df_mice)

train_mice <- cbind(train$Channel, train_mice)
colnames(train_mice)[1] <- 'Channel'
test_mice <- cbind(test$Channel, test_mice)
colnames(test_mice)[1] <- 'Channel'
df_mice <- cbind(customers$Channel, df_mice)
colnames(df_mice)[1] <- 'Channel'

split_impute(train_mice, test_mice)
impute_split(df_mice)



#3.KNN
library(DMwR)
train_knn <- knnImputation(train_MNAR[,-1])
test_knn <- knnImputation(test_MNAR[,-1])
df_knn <- knnImputation(df_MNAR[,-1])

train_knn <- cbind(train$Channel, train_knn)
colnames(train_knn)[1] <- 'Channel'
test_knn <- cbind(test$Channel, test_knn)
colnames(test_knn)[1] <- 'Channel'
df_knn <- cbind(customers$Channel, df_knn)
colnames(df_knn)[1] <- 'Channel'

split_impute(train_knn, test_knn)
impute_split(df_knn)

#4.missForest
library(missForest)
set.seed(333)
train_miss <- missForest(train_MNAR[,-1])$ximp
test_miss <- missForest(test_MNAR[,-1])$ximp
df_miss <- missForest(df_MNAR[,-1])$ximp

train_miss <- cbind(train$Channel, train_miss)
colnames(train_miss)[1] <- 'Channel'
test_miss <- cbind(test$Channel, test_miss)
colnames(test_miss)[1] <- 'Channel'
df_miss <- cbind(customers$Channel, df_miss)
colnames(df_miss)[1] <- 'Channel'

split_impute(train_miss, test_miss)
impute_split(df_miss)


#########################
#####     MAR     ######
#########################

df_MAR <- data.frame(customers)
col
a1 <-0; a2 <- 1

set.seed(1) #621

df_MAR$Fresh <- ifelse(a1 * scale(customers$Fresh) +a2 * df_MAR$Channel
                        + rnorm(n,-1.15) > 0, NA, df_MAR$Fresh)
df_MAR$Milk <- ifelse(a1 * scale(customers$Milk) +a2 * df_MAR$Channel
                       + rnorm(n,-1) > 0, NA, df_MAR$Milk)
df_MAR$Grocery <- ifelse(a1 * scale(customers$Grocery) +a2 * df_MAR$Channel
                          + rnorm(n,-1) > 0, NA, df_MAR$Grocery)
df_MAR$Frozen <- ifelse(a1 * scale(customers$Frozen) +a2 * df_MAR$Channel
                         + rnorm(n,-1.15) > 0, NA, df_MAR$Frozen)
sum(is.na(df_MAR))
colSums(is.na(df_MAR))

train_MAR <- df_MAR[intrain,]; test_MAR <- df_MAR[-intrain,]

n_train_miss <- sum(is.na(train_MAR)) #444
n_test_miss <- sum(is.na(test_MAR)) #175
n_total_miss <- n_train_miss + n_test_miss #619
# write.csv(train_MAR, 'train_MAR.csv', row.names = F)
# write.csv(test_MAR, 'test_MAR.csv', row.names = F)


train_MAR <- read.csv('train_MAR.csv')
test_MAR <- read.csv('test_MAR.csv')
df_MAR <- rbind(train_MAR, test_MAR)



#1.mean Imputation
train_mean <- data.frame(train_MAR)
test_mean <- data.frame(test_MAR)
df_mean <- data.frame(df_MAR)

for(i in 3:6) {
  train_mean[ , i][is.na(train_mean[ , i])] <- mean(train_mean[ , i], na.rm = TRUE)
  test_mean[ , i][is.na(test_mean[ , i])] <- mean(test_mean[ , i], na.rm = TRUE)
  df_mean[ , i][is.na(df_mean[ , i])] <- mean(df_mean[ , i], na.rm = TRUE)
}

split_impute(train_mean, test_mean)
impute_split(df_mean)


#2.MICE
library(mice)
train_mice <- mice(train_MAR[,-1],seed = 333); train_mice <- complete(train_mice)
test_mice <- mice(test_MAR[,-1],seed = 333); test_mice <- complete(test_mice)
df_mice <- mice(df_MAR[,-1],seed = 333); df_mice <- complete(df_mice)

train_mice <- cbind(train$Channel, train_mice)
colnames(train_mice)[1] <- 'Channel'
test_mice <- cbind(test$Channel, test_mice)
colnames(test_mice)[1] <- 'Channel'
df_mice <- cbind(customers$Channel, df_mice)
colnames(df_mice)[1] <- 'Channel'

split_impute(train_mice, test_mice)
impute_split(df_mice)



#3.KNN
library(DMwR)
train_knn <- knnImputation(train_MAR[,-1])
test_knn <- knnImputation(test_MAR[,-1])
df_knn <- knnImputation(df_MAR[,-1])

train_knn <- cbind(train$Channel, train_knn)
colnames(train_knn)[1] <- 'Channel'
test_knn <- cbind(test$Channel, test_knn)
colnames(test_knn)[1] <- 'Channel'
df_knn <- cbind(customers$Channel, df_knn)
colnames(df_knn)[1] <- 'Channel'

split_impute(train_knn, test_knn)
impute_split(df_knn)

#4.missForest
library(missForest)
set.seed(333)
train_miss <- missForest(train_MAR[,-1])$ximp
test_miss <- missForest(test_MAR[,-1])$ximp
df_miss <- missForest(df_MAR[,-1])$ximp

train_miss <- cbind(train$Channel, train_miss)
colnames(train_miss)[1] <- 'Channel'
test_miss <- cbind(test$Channel, test_miss)
colnames(test_miss)[1] <- 'Channel'
df_miss <- cbind(customers$Channel, df_miss)
colnames(df_miss)[1] <- 'Channel'

split_impute(train_miss, test_miss)
impute_split(df_miss)
