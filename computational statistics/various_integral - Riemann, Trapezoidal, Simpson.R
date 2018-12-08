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

x<-seq(0,5,0.01)

###fun1
fun1 <- function(x) (x-2)^2
plot(x,fun1(x), type = 'l', main = 'fun1')
abline(v = c(2,4),col = "red", lwd = 2)

myRiemann(fun1, 2, 4)
myTrape(fun1, 2, 4)
mySimps(fun1, 2, 4)

system.time(myRiemann(fun1, 2, 4))
system.time(for (i in 1:10) myTrape(fun1, 2, 4))/10
system.time(for (i in 1:100) mySimps(fun1, 2, 4))/100

###fun2
fun2 <- function(x) (x^2+5*x+4*x^3)/sqrt(abs(x) + 4*x^5)
plot(x,fun2(x), type = 'l', main = 'fun2')
abline(v = c(0.5,2.5),col = "red", lwd = 2)

myRiemann(fun2, 0.5,2.5)
myTrape(fun2, 0.5,2.5)
mySimps(fun2, 0.5,2.5)

system.time(myRiemann(fun2, 0.5,2.5))
system.time(for (i in 1:10) myTrape(fun2,0.5,2.5))/10
system.time(for (i in 1:1000) mySimps(fun2, 0.5,2.5))/1000

###fun3
fun3 <- function(x) sin((1+x^4)/(1+(x^6)))*log(x)
plot(x,fun3(x), type = 'l', main = 'fun3')
abline(v = c(2,4),col = "red", lwd = 2)

myRiemann(fun3, 2, 4)
myTrape(fun3, 2, 4)
mySimps(fun3, 2, 4)

system.time(myRiemann(fun2, 2, 4))
system.time(for (i in 1:10) myTrape(fun2, 2, 4))/10
system.time(for (i in 1:1000) mySimps(fun2, 2, 4))/1000