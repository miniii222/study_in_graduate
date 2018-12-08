#미분을 위한 library
library(numDeriv)

g1<-function(x){log(x)/(1+x)}
g2<-function(x){cos(x)+2*sin(x)+x^2}
g3<-function(x){x^3-4*x^2-11*x+30}


#########mybisec, mynewton, mysecant : vector to vector function

#bisection
mybisec <- function(fun,a,b,e=10^(-10)){
  
    n_a<-length(a); n_b<-length(b)
    result<-list()
    
    if(n_a!=n_b){
      warning('length of vector a and vector b are differnet')
      }
  
    else{
      
      x_star<-c()
      maxiter<-1000

      for(i in 1:n_a){
        
        err<-1
        niter<-0
        x0<-(a[i]+b[i])/2
        
    while(niter<=maxiter && err>e){
        
      if(genD(fun,a[i])$D[1,1] * genD(fun,x0)$D[1,1] <0){ b[i]<-x0 
        
      }else{ a[i]<-x0 }
      
        oldx0<-x0; x0<-(a[i]+b[i])/2
        err<-abs(oldx0-x0)
        niter<-niter+1
        }
      

        result$'x_star'[i]<-x0		#찾아낸 값
        result$'err'[i]<-err		#오차
        result$'g(x)'[i]<-fun(x0)		#함수값
        result$'niter'[i]<-niter		#반복횟수
      }
  
      return(result)

      }

}

mybisec(g1,3,5)
mybisec(g1,c(3,2,4),c(4,4,5))
mybisec(g2,5,-5)
mybisec(g3,2,5)

#newton
mynewton<-function(fun,x0,e=10^(-10)){
  
    maxiter<-1000
    n<-length(x0)
    result<-list()
    
    for (i in 1:n){
      err<-1
      niter<-0
      
      while(niter<=maxiter && err>e){
        
        h_t <- -genD(fun,x0[i])$D[1,1]/genD(fun,x0[i])$D[1,2]
        oldx0<-x0[i]; x0[i]<-oldx0+h_t
        
        if(is.na(x0[i])) break;	#Newton 알고리즘 특성상, 분모가 0이 되면 안 되므로
        
        err<-abs(oldx0-x0[i])
        niter<-niter+1
      }
      
      result$'x_star'[i]<-x0[i]
      result$'err'[i]<-err
      result$'g(x)'[i]<-fun(x0[i])
      result$'niter'[i]<-niter
    }
    
    return(result)
}

mynewton(g1,3)
mynewton(g2,5)
mynewton(g3,2)

#secant
mysecant<-function(fun,x0,x1,e=10^(-10)){
  
  
  if(length(x0)!=length(x1)) {
      
    warning('length of vector a and vector b are differnet')
  
    }else{
  
    maxiter<-1000
    result<-list()
    
    for ( i in 1:length(x0)){
        err<-1
        niter<-0
      
      while(niter<=maxiter && err>e){
    
        oldx0<-x0[i]; oldx1<-x1[i]
        oldx0_d<-genD(fun,x0[i])$D[1]; oldx1_d<-genD(fun,x1[i])$D[1]
    
        x1[i]<-oldx1-oldx1_d*(oldx1-oldx0)/(oldx1_d-oldx0_d)
        x0[i]<-oldx1
    
        err<-abs(x1[i]-x0[i])
        niter<-niter+1
      }
        result$'x_star'[i]<-x1[i]
        result$'err'[i]<-err
        result$'g(x)'[i]<-fun(x1[i])
        result$'niter'[i]<-niter
        
      }
  return(result)
}
}

mysecant(g1,3,5)
mysecant(g2,5,-5)
mysecant(g3,2,5)


########소요시간 측정
##g1
system.time(for(i in 1:1000) mybisec(g1,3,5))/1000
system.time(for(i in 1:1000) mynewton(g1,3))/1000
system.time(for(i in 1:1000) mysecant(g1,3,5))/1000
##g2
system.time(for(i in 1:1000) mybisec(g2,5,-5))/1000
system.time(for(i in 1:1000) mynewton(g2,5))/1000
system.time(for(i in 1:1000) mysecant(g2,5,-5))/1000
##g3
system.time(for(i in 1:1000) mybisec(g3,2,5))/1000
system.time(for(i in 1:1000) mynewton(g3,2))/1000
system.time(for(i in 1:1000) mysecant(g3,2,5))/1000


#2.1
x<-c(1.77, -0.23,2.76, 3.80, 3.47, 56.75, -1.34, 4.24, -2.44, 3.29, 3.71, -2.40, 4.53, -0.07, -1.05,
-13.87, -2.53, -1.75, 0.27, 43.21)


#a.
theta<-seq(-3,5, length.out=100); n<-length(theta)

log_like<-c()
for(i in 1:n) log_like[i]<--n*log(pi)-sum(log((x-theta[i])^2+1))

plot(theta, log_like,
     main="log-likelihood function of Cauchy distribution")

log_like_cauchy <- function(theta) sum(-log(pi)-log(1+(x-theta)^2))

x0<-c(-11, -1, 0, 1.5, 4, 4.7, 7, 8,38) #starting points


mynewton(log_like_cauchy,x0)
mynewton(log_like_cauchy,mean(x))

#b.
mybisec(log_like_cauchy,-1,1)
mybisec(log_like_cauchy,1,3)
mybisec(log_like_cauchy,2,4)
#d.
mysecant(log_like_cauchy,-2,-1)
mysecant(log_like_cauchy,-3,3)
mysecant(log_like_cauchy,1,3)

#e.
x<-rnorm(20)
theta<-seq(-3,3, length.out=100); n<-length(theta)

log_like<-c()
for(i in 1:n) log_like[i]<--n*log(2*pi)-sum(0.5*(x-theta[i])^2)

plot(theta, log_like,
     main = "log-likelihood function of Normal distribution")

log_like_normal<-function(theta){
  
  sum (- log(2*pi)/2-0.5*(x - theta)^2 )
  
}

mybisec(log_like_normal,-1,1)
mybisec(log_like_normal,2,3)
mybisec(log_like_normal,5,7)

mynewton(log_like_normal,0)
mynewton(log_like_normal,5)
mynewton(log_like_normal,10)

mysecant(log_like_normal,-1,1)
mysecant(log_like_normal,2,3)
mysecant(log_like_normal,7,10)


#2.2
x<-c(3.91, 4.85,2.28, 4.06, 3.70, 4.04, 5.46, 3.53, 2.28, 1.96, 2.53, 3.88, 2.22, 3.47, 4.82, 2.46, 2.99,
      2.54, 0.52, 2.50)
n<-length(x)

theta<-seq(-pi,pi, length.out = length(x))
log_like<-c()
for(i in 1:n)log_like[i]<-{sum(log(1-cos(x-theta[i])))-n*log(2*pi)}


##a)
plot(theta, log_like, main = 'log-likelihood function')

##b)
mean_x<-mean(x)
fx<-function(x){(1-cos(x-mean_x))/(2*pi)}
mme<-sum(x*fx(x))		#method of moments estimator


##c)
log_like_fx<-function(theta){
  sum(-log(2*pi)+log(1-cos(x-theta)))
}
  
mynewton(log_like_fx,mme)
mynewton(log_like_fx,2.7)
mynewton(log_like_fx,-2.7)

##d)
aa<-seq(-pi,pi,length.out=200)
tb1<-table(round(mynewton(log_like_fx,aa)$'x_star',7))
tb1_count<-as.vector(tb1)

aa_index<-c(1)

for(i in 2:(length(tb1)*2-1)){
  if(i%%2==0) aa_index[i]<-aa_index[i-1]+tb1_count[i/2]-1
  else aa_index[i]<-aa_index[i-1]+1
}

aa[aa_index]


##e)
bb<-seq(aa[116],aa[117], length.out = 200)
star<-mynewton(log_like_fx, bb);star
bb<-seq(bb[193],bb[194], length.out = 200)
star<-mynewton(log_like_fx, bb);star
bb<-bb[c(174,175)]

mynewton(log_like_fx, bb)
