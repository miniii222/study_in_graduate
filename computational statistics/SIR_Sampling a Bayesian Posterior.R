#eg6.2
x <- c(8,3,4,3,1,7,2,6,2,7); x_ <- mean(x)
m <- 100000; n<-10000 # change m!!!
prior <- rlnorm(m,log(4),0.5); u = runif(m)

##### rejection sampling #####
check <- sapply(prior,function(a){
  sum(dpois(x,a,log=T))-sum(dpois(x,x_,log=T))})
check = exp(check)
condition <- u < check

y<-prior[condition];mean(condition)

xx=seq(0,20,length.out=1000)
ey=dlnorm(xx,log(4),.5)*prod(dpois(x,mean(x)))
plot(xx,ey*10^11,type="l", ylab = "Unnormalized Density X 10^11",
     xlab = "lambda")
qy=dlnorm(xx,log(4),.5)*sapply(xx,function(lambda){prod(dpois(x,lambda))})
lines(xx,qy*10^11,lty=2)
legend("topright", legend=c("envelope", "target"),lty = c(1,2))
abline(v = 4.3)

##### SIR #####
w<-sapply(prior,function(a) sum(dpois(x,a,log=T))); w<-exp(w)
sir_<-sample(prior,n,replace = T, prob = w)

hist(sir_, probability = T)
hist(y, probability = T)

length(y); length(sir_)
