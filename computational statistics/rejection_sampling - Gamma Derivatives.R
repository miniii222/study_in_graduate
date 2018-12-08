eg6.1
library(ggplot2); library(ggpmisc)
# r = gamma dist shape parameter (r >= 1)
# n = sample size
# g = function for generating gamma(r,1) draw

qq<-function(r,n){

accept_rate<-c()
z = rnorm(n); u = runif(n)
a <- r - (1/3); b <- 1/sqrt(9*a)
t <- function(y,r) return(a*(1+b*y)^3)

condition <- u<=exp(z*z/2+a*log(t(z,r)/a) - t(z,r)+a)
condition<-ifelse(is.na(condition), FALSE, condition)
gamma<-t(z[condition],r)
accept_rate<-mean(condition); accept_rate

p<-rank(gamma)/(length(gamma)+1)
dt1<-data.frame(x = gamma, p = p)

ggplot(data = dt1, aes(qgamma(p,r),x))+geom_point()+
  geom_smooth(method='lm',se=F)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., sep = "~~~")),
               rr.digits=5,parse = TRUE)+
  ggtitle(paste0('gamma(',r,')',' QQplot'))
}

library(gridExtra)
grid.arrange(qq(4,5000), qq(3,5000),qq(2,5000),qq(1,5000), ncol=2)


x<-seq(0,15,length.out = 100)
plot(x,dgamma(x,shape=4), type = 'l')
