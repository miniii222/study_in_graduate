myroute<-function(x){
  m<-matrix(c(	1,x[1],x[2],x[3],x[4],0,0,0,0,0,
               x[1],1,x[5],0,0,x[8],x[9],0,0,0,
               x[2],x[5],1,x[6],0,0,x[10],0,0,0,
               x[3],0,x[6],1,x[7],0,0,x[11],0,0,
               x[4],0,0,x[7],1,0,0,x[12],x[13],0,
               0,x[8],0,0,0,1,x[14],0,0,x[17],
               0,x[9],x[10],0,0,x[14],1,x[15],0,x[18],
               0,0,0,x[11],x[12],0,x[15],1,x[16],x[19],
               0,0,0,0,x[13],0,0,x[16],1,x[20],
               0,0,0,0,0,x[17],x[18],x[19],x[20],1),ncol=10,byrow=T)
  res<-ifelse((m%*%m%*%m%*%m%*%m%*%m%*%m%*%m%*%m)[1,10]>0,1,0)
  return(res)
}


p<-0.05
n=100000; numedges=20
failed.orig=rep(FALSE,n); failed.is=rep(FALSE,n) 


breaks_p<-sample(c(T,F),numedges*n,prob=c(p,1-p),replace=T)
m1<-matrix(breaks_p, ncol = numedges)

for(j in 1:n){
  m1_row <- m1[j,]
  if (sum(m1_row)>0) failed.orig[j] <- myroute(m1_row)
}

pstar<-0.5
breaks_pstar<-sample(c(T,F),numedges*n,prob=c(pstar,1-pstar),replace=T)
m1<-matrix(breaks_pstar, ncol = numedges)

for(j in 1:n){
  m1_row <- m1[j,]
  if (sum(m1_row)>0) failed.is[j] <- myroute(m1_row)
}

sum(failed.orig);sum(failed.is)


bx<-apply(m1,1,sum)
w<-bx*(log(p)-log(pstar))+(numedges-bx)*(log(1-p)-log(1-pstar))
w=exp(w)

mean(failed.is*w)
var(failed.is*w)/n