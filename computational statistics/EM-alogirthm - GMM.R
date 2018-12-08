library(ggplot2)

#Gaussian Mixture Model
X <- c(rnorm(500,1,1),rnorm(500,5,1))
ggplot()+geom_histogram(aes(X, ..density..),
                        colour = "black",fill = "white") 

##loglikelihood convergence
mygmm_lik<-function(X,e=10^(-10),maxiter=1000){
  
  p=0.5; n<-length(X);niter<-0
  sigma1<-sd(X); sigma2<-sd(X);err<-1
  mu1<-X[sample(n,1)];mu2<-X[sample(n,1)]
  
  loglike <- sum(log(p*dnorm(X,mu1,sigma1) + (1-p)*dnorm(X,mu2,sigma2)))
  
  while (err>e && niter<=maxiter) {
    
    # E-step
    y <-p*dnorm(X,mu1,sigma1)/(p*dnorm(X,mu1,sigma1)+(1-p)*dnorm(X,mu2,sigma2))
    
    # M-step
    p <- mean(y)
    mu1 <- sum(y*X)/sum(y); mu2 <- sum((1-y)*X)/sum(1-y)
    sigma1 <-sqrt(sum(y*(X-mu1)^2)/sum(y))
    sigma2 <-sqrt(sum((1-y)*(X-mu2)^2)/sum(1-y))
    
    new_loglike<-sum(log(p*dnorm(X,mu1,sigma1) + (1-p)*dnorm(X,mu2,sigma2)))
  
    err<-abs(loglike-new_loglike); niter<-niter+1
    loglike<-new_loglike
  }
  
  result <- list(pi = p,
                 membership = ifelse(y>=p,1,2),
                 mean = c(mu1,mu2),
                 sd = c(sigma1,sigma2),
                 niter = niter)
  
  return(result)
}
mygmm_lik(X)

##pi convergence
mygmm_p <- function(X,e=10^(-10),maxiter=1000){
  
  p=0.5; n<-length(X);niter<-0
  sigma1<-sd(X); sigma2<-sd(X);err<-1
  mu1<-X[sample(n,1)];mu2<-X[sample(n,1)]
  
  err<-1;niter<-0
  
  while(niter<=maxiter && err>e){
    
    #E-step
    y <-p*dnorm(X,mu1,sigma1)/(p*dnorm(X,mu1,sigma1)+(1-p)*dnorm(X,mu2,sigma2))
    
    # M-step
    new_p <- mean(y)
    mu1 <- sum(y*X)/sum(y); mu2 <- sum((1-y)*X)/sum(1-y)
    sigma1 <-sqrt(sum(y*(X-mu1)^2)/sum(y))
    sigma2 <-sqrt(sum((1-y)*(X-mu2)^2)/sum(1-y))
    
    niter = niter + 1
    err = abs(new_p - p)
    p = new_p
  }
  
  result <- list(pi = p,
                 membership = ifelse(y>=p,1,2),
                 mean = c(mu1,mu2),
                 sd = c(sigma1,sigma2),
                 niter = niter)
  
  return(result)
}

mygmm_p(X)

plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}

data.frame(x = X) %>% ggplot() +
  geom_histogram(aes(x, ..density..), colour = "black",fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mygmm_p(X)$mean[1], mygmm_p(X)$sd[1], 
                            lam = mygmm_p(X)$pi),
                colour = "red", lwd = 1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mygmm_p(X)$mean[2], mygmm_p(X)$sd[2], 
                            lam = 1-mygmm_p(X)$pi),
                colour = "blue", lwd = 1.5) +
  ylab("Density") +xlab("Values") + ggtitle("Final GMM Fit")