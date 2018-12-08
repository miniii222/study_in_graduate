########ex6.13
S0=100; r=0.05; sigma = 0.3; T=50; K=102
m<-10000; n<-1000
A<-c() #Asian call option
theta<-c()
mu.mc<-c(); theta.mc<-c()
for(j in 1:m){
for (i in 1:n){
  S<-cumprod(c(S0,exp((r-sigma*sigma/2)/365+sigma*rnorm(T)/sqrt(365))))
  
  #Arithmetric mean S
  A[i] <- exp(-r*T/365)*max(0,mean(S[2:length(S)])-K)
  #Geometric mean S
  theta[i] <- exp(-r*T/365)*max(0,exp(mean(log(S[2:length(S)])))-K)
}

mu.mc[j]<-mean(A); theta.mc[j] <- mean(theta)
}

sd(mu.mc)

c3<-1 + 1/T
c2<-sigma*((c3*T/1095)*(1 + 1/(2*T)))^.5
c1<-(1/c2)*(log(S0/K) + (c3*T/730)*(r - (sigma^2)/2) +
               (c3*(sigma^2)*T/1095)*(1 + 1/(2*T)))
theta <- S0*pnorm(c1)*exp(-T*(r + c3*(sigma^2)/6)*(1 - 1/T)/730) -
  K*pnorm(c1-c2)*exp(-r*T/365)

mu.cv<-mu.mc-1*(theta.mc-theta)
sd(mu.mc); sd(mu.cv)
# 0.1086414 / 0.00291093

mu.cv<-mu.mc-1.0217*(theta.mc-theta)
sd(mu.mc); sd(mu.cv)


##############6.3
m <- 100000
#envelope function normal
qx <- function(x) exp((-abs(x)^3)/3) #target function
c<-integrate(qx,-Inf,Inf)$value
qx2 <- function(x) exp((-abs(x)^3)/3) / c #pdf
hx <- function(x) x^2

x<-c(-50,50,by = 0.1)
plot(x, qx2(x), type = 'l')

##a Importance Sampling
z<-rnorm(m)
w_star <- qx2(z) / dnorm(z)
mu_IS <- sum(hx(z) * w_star / sum(w_star))

##b rejection sampling
u <- runif(m)
condition <- u<=qx2(z)/dnorm(z); mean(condition)
mu_rs<-mean(hx(z[condition]))


xx<-seq(-5,5,0.01)
plot(xx,qx2(xx),type="l",main="Rejection Sampling",xlab="x",ylab="qx")
lines(xx,dnorm(xx),type="l",lwd=2,lty=2)
legend(2,0.3,c("target","env"),lty=1:2,lwd=2)


## c
X_sort <- sort(z[condition]); n<-length(X_sort)
mu_c <- sum((X_sort[2:n]-X_sort[1:n-1])*
              hx(X_sort[1:n-1])*qx(X_sort[1:n-1]))/
        sum((X_sort[2:n]-X_sort[1:n-1])*qx(X_sort[1:n-1]))

## d
rep.n<-100
result <- matrix(nrow=rep.n, ncol=2)

#rejection sampling
system.time(
for(j in 1:rep.n){
  z<-rnorm(m);u <- runif(m)
  condition <- u<=qx2(z)/dnorm(z); mean(condition)
  mu_rs<-mean(hx(z[condition]))
  result[j,1] <- mu_rs
}
)/rep.n

#P and R
system.time(
for(j in 1:rep.n){
  X_sort <- sort(z[condition]); n<-length(X_sort)
  mu_c <- sum((X_sort[2:n]-X_sort[1:n-1])*
                hx(X_sort[1:n-1])*qx(X_sort[1:n-1]))/
    sum((X_sort[2:n]-X_sort[1:n-1])*qx(X_sort[1:n-1]))
  result[j,2] <- mu_c
}
)/rep.n

colMeans(result)
apply(result,2,var)
apply(result,2,sd)


#############6.6
m <- 5000; n <- 100
h_z<-c(); mu.mc<-c()

for(j in 1:m){
  for(i in 1:n){
    x_pois<-rpois(25,2) #random sampling
    z <- (mean(x_pois)-2)/sqrt(2/25)
    h_z[i] <- ifelse(z>=1.645,1,0)
}
  mu.mc[j]<-mean(h_z)
}

#a
####Naive MC
mean(mu.mc) #0.055434
sort(mu.mc)[c(1,0.95*m)] #95% CI
var(mu.mc)

####Importance sampling
e <- rpois(m,lambda = 2.4653) #envelope function normal
q <- function(x) dpois(x, lambda = 2) #target function

w<-q(e)/dpois(e,lambda = 2.4653) #weight
mu_IS <- mean(q(e)*w)


#b
lambda <- seq(2.2,4, length.out = 5)
m<-5000
power_df<-matrix(rep(0,5*m), ncol = 5)

for (j in 1:5){
  
  for(i in 1:m){
      x_pois<-rpois(25,lambda = lambda[j]) #random sampling
      z <- (mean(x_pois)-lambda[j])/sqrt(lambda[j]/25)
      power_df[i,j] <- 1-pnorm(q=qnorm(0.95)-z)
    }
 
}

power_df<-data.frame(power_df)
colnames(power_df)<-c('lambda1','lambda2','lambda3',
                      'lambda4','lambda5' )

boxplot(power_df)

######6.7
S0=50; K=52; sigma = 0.5; T=30; r=0.05

#a
#naive mc
n<-100000
c.mc<-c()
for (i in 1:n){
  St<-S0*exp( (r-sigma*sigma/2)*T/365 + sigma*rnorm(1)*sqrt(T/365) )
  c.mc[i]<-exp(-r*T/365)*max(0,St-K)
}
mean(c.mc)

#b
m<-10000; n<-1000
A<-c() #Asian call option
theta<-c()
mu.mc<-c(); theta.mc<-c()
for(j in 1:m){
  for (i in 1:n){
    S<-cumprod(c(S0,exp((r-sigma*sigma/2)/365+sigma*rnorm(T)/sqrt(365))))
    
    #Arithmetric mean S
    A[i] <- exp(-r*T/365)*max(0,mean(S[2:length(S)])-K)
    #Geometric mean S
    theta[i] <- exp(-r*T/365)*max(0,exp(mean(log(S[2:length(S)])))-K)
  }
  
  mu.mc[j]<-mean(A); theta.mc[j] <- mean(theta)
}

mean(mu.mc)
var(mu.mc); sd(mu.mc)
#c
c3<-1 + 1/T
c2<-sigma*((c3*T/1095)*(1 + 1/(2*T)))^.5
c1<-(1/c2)*(log(S0/K) + (c3*T/730)*(r - (sigma^2)/2) +
              (c3*(sigma^2)*T/1095)*(1 + 1/(2*T)))
theta <- S0*pnorm(c1)*exp(-T*(r + c3*(sigma^2)/6)*(1 - 1/T)/730) -
  K*pnorm(c1-c2)*exp(-r*T/365)

mu.cv<-mu.mc-1*(theta.mc-theta)
mean(mu.cv); mean(mu.mc)
#0.944591 0.9441085
sd.cv<-sd(mu.cv); sd.mc<-sd(mu.mc)
var(mu.cv); sd(mu.cv)

#d
m<-5000; n<-1000
A1<-c(); A2<-c()
mu1<-c(); mu2<-c()
for(j in 1:m/2){
  for (i in 1:n){
    S1<-cumprod(c(S0,exp((r-sigma*sigma/2)/365+sigma*rnorm(T)/sqrt(365))))
    S2<-cumprod(c(S0,exp((r-sigma*sigma/2)/365+sigma*(-rnorm(T))/sqrt(365))))
    
    A1[i] <- exp(-r*T/365)*max(0,mean(S1[2:length(S1)])-K)
    A2[i] <- exp(-r*T/365)*max(0,mean(S2[2:length(S2)])-K)
  }
  
  mu1[j]<-mean(A1); mu2[j] <- mean(A2)
}

mu.as<-(mean(mu1) + mean(mu2))/2
var.as<-(var(mu1)+var(mu2))/4+cov(mu1,mu2)/2
