myRomberg<-function(fun,a,b,m,e = 10^(-7)){
  
  n=1; Tn<-(fun(a)+fun(b))/2
  niter<-0; err<-1
  
  for(j in 1:m){
    Tn_new <- 0.5*Tn; h = (b-a)/n
    for(i in 1:n){
    Tn_new<-Tn_new+0.5*h*fun(a+(i-0.5)*h)
    }
  
    err<-abs(Tn_new - Tn)
    Tn <- Tn_new; n<-2*n
    
    niter<-niter+1
  }
  
  # result <-list(niter = niter,integration = Tn,
  #               n = n)
  return(Tn)
}

u<-5
unif1<-function(x) dunif(x,1,u)*(u-1)/x

TT<-list(); 
for(i in 0:6) {
  TT[[i+1]]<-NA
  TT[[1]][i+1]<- myRomberg(unif1, 1,u,i)
}

for(j in 2:7){
for(i in j:7)
  TT[[j]][i]<-((4^(j-1))*TT[[j-1]][i]-TT[[j-1]][i-1])/(4^(j-1)-1)
}

myRomberg(unif1,1,u)
integrate(unif1, 1,u)