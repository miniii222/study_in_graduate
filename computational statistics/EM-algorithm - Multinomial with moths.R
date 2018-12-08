#Multinomial with peppered moths

moth_em<-function(n_C,n_I,n_T,e = 10^(-10)){
  
  p<-1/3; q<-1/3; r<-1/3
  n <- n_C+n_I+n_T; niter<-0
  err<-1

  while( err > e){
    
    #E-step
    n_CC<-(n_C*p^2)/(p^2+2*p*q+2*p*r)
    n_CI<-(n_C*2*p*q)/(p^2+2*p*q+2*p*r)
    n_CT<-(n_C*2*p*r)/(p^2+2*p*q+2*p*r)
    n_II<-(n_I*q^2)/(q^2+2*q*r)
    n_IT<-(n_I*2*q*r)/(q^2+2*q*r)
    
    #M-step
    p_hat <- (2*n_CC + n_CI + n_CT)/(2*n)
    q_hat <- (2*n_II + n_IT + n_CI)/(2*n)
    r_hat <- 1 - p_hat - q_hat
    
    err<-mean(abs(p_hat-p),abs(q_hat-q))
    p<-p_hat; q<-q_hat; r<-r_hat

    niter<-niter+1
  }

  
  # data.frame(p_hat = p_hat, q_hat=q_hat, r_hat = r_hat, niter = niter)
  n_CC<-(n_C*p^2)/(p^2+2*p*q+2*p*r)
  n_CI<-(n_C*2*p*q)/(p^2+2*p*q+2*p*r)
  n_CT<-(n_C*2*p*r)/(p^2+2*p*q+2*p*r)
  n_II<-(n_I*q^2)/(q^2+2*q*r)
  n_IT<-(n_I*2*q*r)/(q^2+2*q*r)
  
  c(n_CC,n_CI,n_CT,n_II,n_IT)
}

moth_em(85,196,341)
