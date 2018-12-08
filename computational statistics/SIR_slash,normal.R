m <- 100000; n <- 5000
z <- rnorm(m); s <- rnorm(m) / runif(m)

##### target : slash #####
dslash<-function(x){
  
  a<-ifelse(x==0, 1/(2*sqrt(2*pi)), (1-exp(-0.5*x^2)) / (sqrt(pi*2)*x*x))
  
  return(a)
}

w1 <- function(x){
  
  weight <- dslash(x) / dnorm(x)
  weight <- weight / sum(weight)
  
  return(weight)
}

myslash<-sample(z,n,replace = TRUE, prob=w1(z))

hist(myslash, freq = FALSE, xlim =c(-4,4))
lines(seq(-4,4,by=.01),dslash(seq(-4,4,by=.01)))

##### target : normal #####
w2 <- function(x){
  weight <- dnorm(x) / dslash(x)
  weight <- weight / sum(weight)
  return(weight)
}

mynorm<-sample(s,n,replace = TRUE, prob=w2(s))

hist(mynorm, freq = FALSE, xlim =c(-4,4))
lines(seq(-4,4,by=.01),dnorm(seq(-4,4,by=.01)))