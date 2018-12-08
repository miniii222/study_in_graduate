#Multinomial with Complex cell structure
bt_em<-function(n_O,n_A,n_B,n_AB,e=10^(-10)){

n <- n_O + n_A + n_B + n_AB
p <- 1/3; q<- 1/3; r<-1/3
err1<-1; err2<-1
niter<-1

while(err1>e & err2>e){
  
#E-step
n_AA<-n_A*(p^2/(p^2+2*p*r))
n_AO<-n_A*(2*p*r/(p^2+2*p*r))
n_BB<-n_B*(q^2/(q^2+2*q*r))
n_BO<-n_B*(2*q*r/(q^2+2*q*r))

#M-step
p_hat <- (n_AA + 0.5*n_AO + 0.5*n_AB)/n
q_hat <- (n_BB + 0.5*n_BO + 0.5*n_AB)/n

err1<-abs(p_hat-p); err2<-abs(q_hat-q)

p<-p_hat; q<-q_hat
niter<-niter+1
}

r_hat <- 1-p_hat-q_hat

data.frame(p_hat = p_hat, q_hat=q_hat, r_hat = r_hat,n = niter)
}

bt_em(176,182,60,17)
