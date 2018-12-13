library(dplyr)
library(readr)
binary<-read_csv('binary.csv')
sat<-read_csv('sat.csv')

manual_logistic_regression = function(X,y,threshold = 1e-10, max_iter = 100)
{
  calc_p = function(X,beta)
  {
    beta = as.vector(beta)
    return(exp(X%*%beta) / (1+ exp(X%*%beta)))
  }  
  
  #initial guess for beta
  beta = rep(0,ncol(X))
  diff = 10000 
  iter_count = 0
  
  #### iterative bit ####
  while(diff > threshold )
  {
    
    p = as.vector(calc_p(X,beta))
    W =  diag(p*(1-p)) 
    
    #calculate the change in beta
    std <- solve(t(X)%*%W%*%X)
    beta_change = solve(t(X)%*%W%*%X) %*% t(X)%*%(y - p)
    
    #update beta
    beta = beta + beta_change
    
    diff = sum(beta_change^2)
    
    #see if we've hit the maximum number of iterations
    iter_count = iter_count + 1
    if(iter_count > max_iter) {
      stop("This isn't converging, mate.")
    }
  }
  
  coef = c("(Intercept)" = beta[1], 'x1' = beta[2])
  
  y_hat = coef[1]+coef[2]*y
  e = y-y_hat
  std = c(sqrt(std[1,1]), sqrt(std[2,2]))
  z<-coef / std
  
  #result
  result = list()
  result$'Deviance Residuals' = quantile(e)
  result$'Estimate' = coef
  result$'Std. Error' = std
  result$'z_value' = z
  result$'Pr(>|z|)' = (1-pnorm(abs(z)))*2
  return(result)
}

##logistic
glm2<-glm(admit~gre, data = binary, family = binomial)
# (Intercept)          gre  
# -2.901344     0.003582

X<-model.matrix(~gre, data = binary)
y<-matrix(binary$admit)

manual_logistic_regression(X,y)

