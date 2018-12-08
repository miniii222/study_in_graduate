###5.3
#여러 가지 적분을 구하는 함수들은 various_integral - Riemann, Trapezoidal, Simpson.R 파일에 있는 코드 이용
#####a
dlnorm(xx,log(4),.5)*prod(dpois(x,mean(x)))

x1<-c(6.52,8.32,0.31,2.82,9.96,0.14,9.64)
f1<-function(x) dcauchy(x,5,2)*dnorm(x,mean(x1), sqrt(9/7))*7.84654

myRiemann<-function(fun,a,b,e = 10^(-7)){
  
  n=1; Rn<-100
  niter<-0; err<-1
  
  while(err > e){
    Rn_new<-0; h = (b-a)/n
    for(i in 0:(n-1)) Rn_new<-Rn_new + h*fun(a+i*h)
    
    err<-abs(Rn_new - Rn)
    Rn <- Rn_new; n<-2*n
    
    niter<-niter+1
  }
  
  result <-list(niter = niter,integration = Rn)
  return(result)
}
myTrape<-function(fun,a,b,e = 10^(-7)){
  
  n=2; Tn<-100
  niter<-0; err<-1
  
  while(err > e){
    h = (b-a)/n
    Tn_new <- (fun(a)+fun(b))*h/2
    for(i in 1:(n-1)) Tn_new<-Tn_new + h*fun(a+i*h)
    
    err<-abs(Tn_new - Tn)
    Tn <- Tn_new; n<-2*n
    
    niter<-niter+1
  }
  
  result <-list(niter = niter,integration = Tn)
  return(result)
}
mySimps<-function(fun,a,b,e = 10^(-7)){
  
  n=2; Sn<-100
  niter<-0; err<-1
  
  while(err > e){
    Sn_new <- 0; h = (b-a)/n
    for(i in 1:(n/2)){
      x0<-a+2*(i-1)*h ;x2<-a+2*i*h; x1<-(x0+x2)/2
      Sn_new<-Sn_new+fun(x0)+4*fun(x1)+fun(x2)
    }
    Sn_new<-Sn_new*h/3
    err<-abs(Sn_new - Sn)
    Sn <- Sn_new; n<-2*n
    
    niter<-niter+1
  }
  
  result <-list(niter = niter,integration = Sn)
  return(result)
}

system.time(for(i in 1:1000) integrate(f1, -Inf, Inf))/1000
system.time(for(i in 1:10) myRiemann(f1,-1000, 1000))/10
system.time(for(i in 1:10) myTrape(f1,-1000, 10))/10
system.time(for(i in 1:10) mySimps(f1,-1000, 1000))/10


###b
system.time(for(i in 1:100) myRiemann(f1,2,8,e=10^(-4)) )/100
system.time(for(i in 1:100) myTrape(f1,2,8,e=10^(-4)) )/100
system.time(for(i in 1:100) mySimps(f1,2,8,e=10^(-4)) )/100

###c
f_c<-function(u){
  mu <- log(u/(1-u))
  return(7.84654*dnorm(mean(x1), mu, 3/sqrt(7))*dcauchy(mu,5,2)/(u*(1-u)))
  }
u<-exp(3)/(1+exp(3))

######1.ignore the singularity 1
system.time(for(i in 1:1000) integrate(f_c,u,1) )/1000
system.time(for(i in 1:10) myRiemann(f_c,u,0.99999) )/10

myTrape(f_c,u,0.99999) #NA
mySimps(f_c,exp(3)/(1+exp(3)),0.99999) #NA

######2. fix the singularity 1
system.time(for(i in 1:1000) integrate(f_c,u,0.99999) )/1000
system.time(for(i in 1:10) myRiemann(f_c,u,0.99999) )/10

system.time(for(i in 1:10) myTrape(f_c,u,0.99999) ) /10
system.time(for(i in 1:10) mySimps(f_c,exp(3)/(1+exp(3)),0.99999) ) /10

###d
f_d<-function(u){
  mu <- 1/u
  return(7.84654*dnorm(mean(x1), mu, 3/sqrt(7))*dcauchy(mu,5,2)/(-u^2))
}

######1.ignore the singularity 0
system.time(for(i in 1:1000) integrate(f_d,0,1/3) )/1000
system.time(for(i in 1:5) myRiemann(f_d,1/3,0) )/5

myTrape(f_d,0,1/3) #NA
mySimps(f_d,0,1/3) #NA

######2. fix the singularity 0
system.time(for(i in 1:1000) integrate(f_d,10^(-7),1/3) )/1000
system.time(for(i in 1:5) myRiemann(f_d,10^(-7),1/3) )/5

system.time(for(i in 1:10) myTrape(f_d,10^(-7),1/3) )/10
system.time(for(i in 1:10) mySimps(f_d,10^(-7),1/3) )/10