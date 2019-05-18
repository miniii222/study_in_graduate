#7
library(ISLR)
set.seed(1)

dsc <- scale(USArrests)
d1 <- dist(dsc)^2
d2 <- as.dist(1-cor(t(dsc)))
summary(d2 / d1)
hist(d2/d1, breaks = 15)

#10
##a
set.seed(2)
x <- matrix(rnorm(20 * 3 * 50, mean = 0, sd = 0.001), ncol = 50)
x[1:20, 2] <- 1
x[21:40, 1] <- 2
x[21:40, 2] <- 2
x[41:60, 1] <- 1

y <- c(rep(1,20), rep(2,20), rep(3,20))

##b
pr.out <- prcomp(x)
plot(pr.out$x[,1:2], col = 1:3)

##c
kmeans1 <- kmeans(x,3, nstart = 20)
table(y, kmeans1$cluster)
plot(km)

##d
kmeans2 <- kmeans(x, 2, nstart = 20)
table(y, kmeans2$cluster)

##e
kmeans3 <- kmeans(x, 4, nstart = 20)
table(y, kmeans3$cluster)

##f
km <- kmeans(pr.out$x[,1:2], 3, nstart = 20)
table(y, km$cluster)

##g
km2 <- kmeans(scale(x),3, nstart = 20)
table(y, km2$cluster)
