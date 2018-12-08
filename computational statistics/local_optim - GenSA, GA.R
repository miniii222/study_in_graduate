library(readr)
baseball <- read_table2("baseball.txt")
p = ncol(baseball)-1

baseball$freeagent <- factor(baseball$freeagent)
baseball$arbitration <- factor(baseball$arbitration)
salary.log <- log(baseball$salary)
baseball.sub = baseball[,-1]

library(GenSA)
aic_sa <- function(theta){
  
  dat <- cbind(baseball.sub[,theta > 0.5], salary.log)
  return(extractAIC(lm(salary.log ~ ., dat))[2])
}

lower <- rep(0, p)
upper <- rep(1, p)

GenSA_result <- GenSA(fn=aic_sa, lower=lower, upper=upper,
                      control = list(maxit = 500))
plot(GenSA_result$trace.mat[,3], type='l',xlim=c(0,2000))
xx<-ifelse(GenSA_result$par >=0.5 ,1,0)
names(baseball.sub)[which(xx==1)]

library(GA)
aic_ga<-function(theta){
  
  x<-c(TRUE,theta==1)
  -extractAIC(lm(log(salary)~., data = baseball[,x]))[2]
}

baseball_ga <- ga(type = "binary",fitness=aic_ga, nBits = p,
                  popSize = 75)

plot(baseball_ga)
summary(baseball_ga)
var_selected<-which(as.vector(summary(baseball_ga)$solution)==1)
colnames(baseball[,-1][,var_selected])