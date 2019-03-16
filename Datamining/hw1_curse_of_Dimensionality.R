#2.
#generate n points p dimensions in the sphere
myftn <- function(n, p){
  
  #n : the number of points
  #p : dimension
  
  mat1 <- matrix(data = rnorm(n*p)  ,ncol = p)
  S <- rowSums(mat1^2)
  
  #points on the surface
  mat1 <- mat1 / sqrt(S)
  
  for(i in 1:n){
  dist1 <- sqrt(qchisq(runif(1,0,pchisq(1, df=p)), p))
  mat1[i,] <- mat1[i,] * dist1
}  #points in the sphere
  
  return(mat1)
}


#2d points in the sphere
plot(myftn(2000,2))

library('scatterplot3d')
#3d points in the sphere
scatterplot3d(myftn(2000,3))


#3.
##Compare 
my_distance <- function(n, sim.n, p){
  
  #n : the number of points
  #sim.n : the number of simulation
  #p : dimension
  
  dist1 <- rep(NA, n)
  min1 <- rep(NA, sim.n)
  
  for(j in 1:sim.n){
  
    for(i in 1:n){
    dist1[i] <- sqrt(qchisq(runif(1, 0, pchisq(1, df=p)), p))
    }
    
    min1[j] <- min(dist1)
  }
  
  return(median(min1))
}

n <- 2000; sim.n <- 1000; p<-3
d_equation <- (1-0.5^(1/n))^(1/p)
d_equation

my_distance(n, sim.n, p)
