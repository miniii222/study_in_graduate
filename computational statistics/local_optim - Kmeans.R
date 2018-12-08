myKmeans <- function(x, K, iter.max = 10, nstart = 1){
  
  colnames(x) <- c("a", "b")
  n <- nrow(x)
  mycl<-list() #cluster numbering
  mymean <- list() #random initial mean
  iter <- c()

  #grouping function
  grouped <- function( K, j, mycl, mymean ) {
    
    niter<-0 #the number of iteration
    m0 <- matrix(rep(0,2*K), ncol=2)
    
    while( niter <= iter.max && !all(mymean[[j]] == m0)){
      
      mycl[[j]] <- which.min(as.vector(dist(rbind(x[1,],mymean[[j]])))[1:K])
      
      for (i in 2:n){
        mycl[[j]][i] <- which.min(as.vector(dist(rbind(x[i,],mymean[[j]])))[1:K])
      } #grouped by using greedy algorithm
      
      a<-aggregate(a~mycl[[j]], data = data.frame(cbind(x,mycl[[j]])), mean)[,2]
      b<-aggregate(b~mycl[[j]], data = data.frame(cbind(x,mycl[[j]])), mean)[,2]
      m0<-mymean[[j]]
      mymean[[j]] <- cbind(a,b)
      
      niter = niter+1
    }
    
    list('cluster' = mycl[[j]], 'centers' = mymean[[j]],
         'iter' = niter)
  }
  
  wss<-list(); wss[[nstart]]<-NA; towss <- c() #initialization
  
  for (j in 1:nstart) {
    mymean[[j]] <- x[sample(n,K),]
    mycl[[j]] <- grouped(K,j,mycl,mymean)[[1]]
    mymean[[j]] <- grouped(K,j,mycl,mymean)[[2]]
    iter[j] <- grouped(K,j,mycl,mymean)[[3]]
    
    for (i in 1:K){
      wss[[j]] <- c(wss[[j]], sum(scale(x[mycl[[j]]==i,], scale = FALSE)^2))
      }
    towss[j] <- sum(wss[[j]], na.rm = T)
  }
 
  nstart.index <- which.min(towss)
  colnames(mymean[[nstart.index]]) <- colnames(x)

  result<-list('centers' = mymean[[nstart.index]],
               'cluster' = mycl[[nstart.index]],
               'totss' = sum(scale(x, scale = FALSE)^2),
               'withiniss' = wss[[nstart.index]][!is.na(wss[[nstart.index]])],
               'tot.withiniss' = towss[nstart.index],
               'size'= as.vector(table(mycl[[nstart.index]])),
               'iter' = iter[nstart.index])
  
  result$'betweenss' <- result[['totss']] - result[['tot.withiniss']]

  return(result)
}


#ex1
x <- rbind(matrix(rnorm(50, sd = 0.3), ncol = 2),
            matrix(rnorm(50, mean = 1, sd = 0.3), ncol = 2))
system.time(for(i in 1:1000) kmeans(x, 2))/1000
system.time(for(i in 1:100) myKmeans(x, 2))/1000

plot(x, col = myKmeans(x, 2)$cluster)
points(myKmeans(x, 2)$centers, col = 1:2, pch = 8, cex = 2)

#ex2
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 0.5, sd = 0.3), ncol = 2))
plot(x)

system.time(for(i in 1:100) kmeans(x, 5, nstart=3))/100
system.time(for(i in 1:100) myKmeans(x,5, nstart=3))/100

plot(x, col = myKmeans(x,5, nstart=3)$cluster)
points(myKmeans(x,5, nstart=3)$centers, col = 1:5, pch = 8, cex = 2)