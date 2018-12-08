library(ggplot2)

#Bivariate Normal with missing values
w1 <- c(8,11,16,18,6,4,20,25,9,13)
w2 <- c(10,14,16,15,20,4,18,22,NA,NA)

binorm_missing<-function(w1,w2,e=10^(-10),maxiter=1000){
  
  mu1 = mean(w1); mu2 = mean(w2, na.rm=T)
  v1<-var(w1); v2<-var(w2, na.rm=T); v12<-cov(w1[1:8],w2[1:8])
  n<-length(w1)
  err<-1; niter<-0
  
  while(niter<=maxiter && err>e){
    
  #E-step
  w2[9] <- mu2 + (v12/v1)*(w1[9] - mu1)
  w2[10] <- mu2 + (v12/v1)*(w1[10] - mu1)

  
  #M-step
  mu2 <- mean(w2);
  new_v12 <- cov(w1,w2); v2 <- var(w2)
  
  err<-abs(new_v12 - v12); v12<-new_v12
  
  niter<-niter+1
  
  }
  
  result<-list(mu = c(mu1,mu2),
               var = matrix(c(v1,v12,v12,v2),ncol=2))
  
  return(result)
}

binorm_missing(w1,w2)
