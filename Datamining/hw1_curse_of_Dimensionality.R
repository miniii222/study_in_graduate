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
plot(myftn(10000,2))

library('scatterplot3d')
#3d points in the sphere
scatterplot3d(myftn(10000,3))
