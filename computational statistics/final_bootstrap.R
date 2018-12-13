setwd("C:/Users/wjssm/Desktop/paper")
#https://www.tandfonline.com/doi/full/10.1081/ETC-120028836
library(readr);library(ggplot2)
gdp <- read_table2("gdp.dat")

N  = 10000 #the number of iteration

#Description: Average percent change in gross domestic product
#for 16 countries for 40 years, 1871-1910.
ggplot(gdp)+geom_line(aes(year, gdpchange))+ylab("GDP change(%)")+
  geom_hline(yintercept = mean(gdp$gdpchange))+
  ggtitle("average percent chage in gross domestic product 1871-1910")

se<-function(x) sd(x) / sqrt(N)

######NON-MOVING BLOCK BOOTSTRAP#####
l = 8 #length of a block
n = nrow(gdp) #40
b = n/l #number of blocks 5
# blocks = arranges the blocks in a matrix for easy manipulation
# xbar   = means from block bootstrap pseudo-datasets


nonmoving_func<- function(x,l,N=10000){
  b = length(x) / l
  blocks <- matrix(x, ncol = l,nrow = b, byrow=T)
  #for(i in 1:b) blocks[i,]<-gdp$gdpchange[(l*i-l+1):(l*i)]
  x_<-c()
  
  for (i in 1:N) {
    block.idx<-sample(b, replace = T)
    x_[i]=mean(blocks[block.idx,])
  }
  
  return(x_)
}

non_result <- nonmoving_func(gdp$gdpchange,8)
hist(non_result, main = "Nonmoving Bootsrapping l=8")
mean(non_result)
se(non_result)
se(nonmoving_func(gdp$gdpchange,4))
hist(nonmoving_func(gdp$gdpchange,4),
     main = "length of a block = 4",xlab = "X_")
mean(nonmoving_func(gdp$gdpchange,4))

ll <- c(1,2,4,5,8,10,20)
ll_se <- c(); ll_m <-c()
for(i in 1:length(ll)){
  ll_se[i]<-se(nonmoving_func(gdp$gdpchange,ll[i]))
  ll_m[i]<-mean(nonmoving_func(gdp$gdpchange,ll[i]))
  
}

plot(ll, ll_l, type = 'b', xlab = "length", ylab = "SE",
     main = "Nonmoving bootstrap standard error by length of a block")

######MOVING BLOCK BOOTSTRAP#####
moving_func <- function(x,l,N=10000){
  blocks <- c(); n = length(x)
  b = as.integer(n/l)
  for(i in 1:(n-l+1)) blocks<-rbind(blocks,x[i:(i+l-1)])
  
  x_ <- c()
  
  for (i in 1:N) {
    block.idx <- sample(nrow(blocks),b,replace=T)
    x_[i]=mean(blocks[block.idx,])
    }
  
  return(x_)

}

move_result <- moving_func(gdp$gdpchange,8)
mean(move_result)
hist(move_result, main = "moving block bootstrap l=8")
se(move_result)

mean(moving_func(gdp$gdpchange,4))
se(moving_func(gdp$gdpchange,4))

ll <- c(2:10)
ll_se <- c(); ll_m <-c()
for(i in 1:length(ll)){
  ll_se[i]<-se(moving_func(gdp$gdpchange,ll[i]))
  ll_m[i]<-mean(moving_func(gdp$gdpchange,ll[i]))
  
}

ll_m
plot(ll, ll_se, type = 'b', xlab = "length", ylab = "SE",
     main = "Moving bootstrap standard error by length of a block")

######Blocks of blocks BOOTSTRAP#####

### EXAMPLE 9.11 TREE RINGS
tree <- read_table2("tree.dat")
#Basal area growth increments for a bristlecone pine tree

ggplot(tree)+geom_line(aes(year,basal))+
  ggtitle("basal area growth increments for the years")
##INITIAL VALUES
acf(tree$basal)
p=3 #number of data points per small block
n=nrow(tree) #452
n.block=n-p+1 #number of big blocks 450
len.block=25 #number of small blocks per big block


##FUNCTION
calc.rhat=function(x) {
  n=nrow(x)
  xwhole=c(x[,1],x[(n-1):n,3])
  xm=mean(xwhole)
  return(sum((x[,1]-xm)*(x[,3]-xm))/sum((xwhole-xm)^2)) }

##ESTABLISH BLOCKS and BLOCKS-OF-BLOCKS
#3*450
p_block<-rbind(tree$basal[1:(n-2)],tree$basal[2:(n-1)],tree$basal[3:n])
bob_result <- matrix(rep(NA, 10000*2), ncol = 2)



#moving block
new_block <- array(NA,c(p, len.block, n.block-len.block+1))
for (i in 1:(n.block-len.block+1)){
  new_block[,,i] = p_block[,i:(i+len.block-1)] #3*25*426
}
new_block=aperm(new_block,c(2,1,3))

##B-O-B BOOTSTRAP
lag2cor=rep(NA,10000)

for (i in 1:10000) {
  take.blocks=sample((n.block-len.block+1),n.block/len.block,replace=T)
  newdat=apply(new_block[,,take.blocks],2,rbind)
  
  lag2cor[i]=calc.rhat(newdat)
  bob_result[i,1] <- mean(newdat)
}

##OUTPUT
sd(lag2cor)
sd(lag2cor)/sqrt(10000)
hist(lag2cor, main = "moving block")

########nonmoving block

new_block <- array(NA,c(p, len.block, n.block/len.block))
for (i in 1:(n.block/len.block)){
  new_block[,,i] = p_block[,(len.block*i-len.block+1):(len.block*i)]
}
new_block=aperm(new_block,c(2,1,3))


##B-O-B BOOTSTRAP
lag2cor=rep(NA,10000)

for (i in 1:10000) {
  take.blocks=sample((n.block/len.block),replace=T)
  newdat=apply(new_block[,,take.blocks],2,rbind)
  lag2cor[i]=calc.rhat(newdat)
  bob_result[i,2] <- mean(newdat)
}

##OUTPUT
sd(lag2cor)
sd(lag2cor)/sqrt(10000)
hist(lag2cor, main = "nonmoving block")
##NOTE: The {tseries} package includes tsbootstrap() which is a
##  convenient tool for some of these methods and includes a bootstrap
##  bias estimate as provided in the text.

###compare with naive block
nonmoving_func(tree$basal,25) #not available
mean(moving_func(tree$basal,25))


mean(nonmoving_func(tree$basal,4))
mean(moving_func(tree$basal,4))

se(bob_result[,1]) #moving bob
se(bob_result[,2]) #nonmoving bob






###########block size

mean(x_-mean(gdp$gdpchange)) #mse

##m fixed
n = nrow(gdp)
m = as.integer(0.25*n); l0 = as.integer(0.05*n)

spb_func <- function(theta){
  
  l1 <- theta
  
  l0 = as.integer(0.05*n)
  blocks <- c()
  for(i in 1:(n-l0+1)) blocks<-rbind(blocks,gdp$gdpchange[i:(i+l0-1)])
  
  x_l0 <- c()
  b = as.integer(n/l0)
  for (i in 1:1000) {
    block.idx <- sample(nrow(blocks),b,replace=T)
    x_l0[i]=mean(blocks[block.idx,])
  }
  
  pi_l0 <- mean(x_l0-mean(gdp$gdpchange))
  
  ##########
  m = as.integer(0.25*n)
  
  BB <- c(); pi_l1 <- c()
  for(j in 1:(n-m+1)){
    
    #new subsampling
    BB<-gdp$gdpchange[j:(j+m-1)]
    
    blocks <-c(); b<-as.integer(n/l1)
    for(i in 1:(m-l1+1)) blocks<-rbind(blocks,BB[i:(i+l1-1)])
    
    x_l1 <- c()
    
    for (i in 1:1000) {
      block.idx <- sample(nrow(blocks),b,replace=T)
      x_l1[i]=mean(blocks[block.idx,])
    }
    
    pi_l1[j] <- mean(x_l1-mean(BB))
  }
  
  mean((pi_l1 - pi_l0)^2)

}

#mse optim
constrOptim(c(l0+1, m), f = spb_func, 
            ui = c(0,-1), ci = c(-n), grad = NULL)

l_m <- optim(par = l0+1, fn = spb_func)$par
l_opt <- l_m*(n/m)^(1/3)
moving_func(l_opt)

seee<-c(); mse <-c()
l_x <- 3:10
for(i in 1:length(l_x)) seee[i] <-  se(moving_func(l=l_x[i],x=gdp$gdpchange))
for(i in 1:length(l_x)) mse[i] <-  0.15-spb_func(l_x[i])

plot(l_x,seee)
plot(l_x, mse)
5*(n/m)^(1/3)
6*(n/m)^(1/3)
#2.jacknife plus bootstrapping

