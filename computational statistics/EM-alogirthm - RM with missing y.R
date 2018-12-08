###Regression Model with Missing Y
library(readr)
library(dplyr)

day <- read_csv("day.csv"); day<-day[c(-1,-2)]

#####linear regression
lm_em<-function(p,dt,e=10^(-5)){
  
n = nrow(dt); niter = 1
na_index = sort(sample(n,as.integer(n*p))) #save na index
real_y = dt[na_index,'cnt']$cnt; dt[na_index,'cnt']<-NA

dt[na_index,'cnt']<-mean(dt$cnt, na.rm=T) #fill NA value with mean
old_y <- dt[na_index,'cnt']; new_y <- rep(0, length(na_index))

###lm
while(sum(abs(old_y-new_y))>e){

  old_y = new_y
  lm1<-lm(cnt~., data = dt)
  new_y = predict(lm1, dt)[na_index]
  dt[na_index,'cnt']<-new_y
  niter = niter + 1
  
}

result <- c(mean(abs(old_y - new_y)), niter, mean((new_y-real_y)^2))
return(result)
}

lm_matrix<-matrix(rep(NA,40),ncol=4)

for (i in 1:10) {
  lm_matrix[i,]<-c(i*0.01,lm_em(i*0.01, day))
}
lm_matrix<-data.frame(lm_matrix)
colnames(lm_matrix)<-c('p','error', 'niter','mae');lm_matrix


library(randomForest)
#####random forest
rf_em<-function(p,dt,e=10^(-5)){
  
  n = nrow(dt); niter = 1
  na_index = sort(sample(n,as.integer(n*p))) #save na index
  real_y = dt[na_index,'cnt']$cnt; dt[na_index,'cnt']<-NA
  
  dt[na_index,'cnt']<-mean(dt$cnt, na.rm=T) #fill NA value with mean
  old_y <- dt[na_index,'cnt']; new_y <- rep(0, length(na_index))
  
  ###randomforest
  while(sum(abs(old_y-new_y))>e){
    
    old_y = new_y
    rf1<-randomForest(cnt~., data = dt)
    new_y = predict(rf1, dt)[na_index]
    dt[na_index,'cnt']<-new_y
    niter = niter + 1
    
  }
  
  result <- c(mean(abs(old_y - new_y)), niter, mean((new_y-real_y)^2))
  return(result)
}

rf_matrix<-matrix(rep(NA,40),ncol=4)

for (i in 1:10) {
  rf_matrix[i,]<-c(i*0.01,rf_em(i*0.01, day))
}
rf_matrix<-data.frame(rf_matrix)
colnames(rf_matrix)<-c('p','error', 'niter','mae');rf_matrix