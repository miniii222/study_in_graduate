###1.Principal Components Analysis

states = row.names(USArrests)
#columns
names(USArrests)

#vastly different mean
apply(USArrests,2, mean)
apply(USArrests,2, var)

pr.out = prcomp(USArrests, scale = T) #perform PCA
#prcomp : centers the variables to have mean zero
#sclae = T : have sd one

names(pr.out)

#prior to implementing
pr.out$center
pr.out$scale

#provide the principal component loadings
pr.out$rotation

dim(pr.out$x)
biplot(pr.out, scale = 0)


pr.out$rotation =- pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)

pr.var = pr.out$sdev^2
pr.var

pve = pr.var/sum(pr.var)
pve

plot(pve, xlab = 'Principal Component', 
     ylab = 'Proportion of Variance Explained', ylim = c(0,1),type = 'b')
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b')

###2.Clustering###
#kmeans
set.seed(2)
x <- matrix(rnorm(50*2), ncol = 2)
x[1:25,1] <- x[1:25,1]+3
x[1:25,2] <- x[1:25,2]-4

km.out <- kmeans(x,2,nstart = 20)
km.out$cluster

plot(x, col = (km.out$cluster+1), main = 'K-means Clustering Results
     with K=2', xlab = '', ylab = '', pch = 20, cex = 2)


set.seed(3)
km.out <- kmeans(x,3,nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x,3, nstart = 20)
km.out$tot.withinss

#hierarchical clustering
hc.complete <- hclust(dist(x), method = 'complete')
hc.average <- hclust(dist(x), method = 'average')
hc.single <- hclust(dist(x), method = 'single')

par(mfrow = c(1,3))
plot(hc.complete ,main =" Complete Linkage ", xlab="", sub ="",
     cex =.9)
plot(hc.average , main =" Average Linkage ", xlab="", sub ="",
       cex =.9)
plot(hc.single , main=" Single Linkage ", xlab="", sub ="",
       cex =.9)

#cut a tree  into groups of data
cutree(hc.complete,2)
cutree(hc.average,2)
cutree(hc.single,2)
cutree (hc.single , 4)

par(mfrow = c(1,1))
xsc <- scale(x)
plot(hclust(dist(xsc), method = 'complete'),
     main = "Hierarchical Clustering with Scaled Features")

###3.NCI60 Data Example
library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)
nci.labs[1:4]
table(nci.labs)

pr.out <- prcomp(nci.data, scale = T)

Cols <- function(vec){
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow = c(1,2))
plot(pr.out$x[,1:2], col = Cols(nci.labs), pch = 19,
     xlab = 'Z1', ylab = 'Z2')
plot(pr.out$x[,c(1,3)], col = Cols(nci.labs), pch = 19,
     xlab = 'Z1', ylab = 'Z3')

summary(pr.out)
plot(pr.out)

pve =100* pr.out$sdev ^2/ sum(pr.out$sdev ^2)
par(mfrow =c(1,2))
plot(pve , type ="o", ylab="PVE ", xlab=" Principal Component ",
       col =" blue")
plot(cumsum (pve ), type="o", ylab =" Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")

sd.data=scale(nci.data)
par(mfrow =c(1,3))
data.dist=dist(sd.data)
plot(hclust (data.dist), labels =nci.labs , main="Complete Linkage",
     xlab ="", sub ="", ylab ="")
plot(hclust (data.dist , method ="average"), labels =nci.labs,
       main="Average Linkage", xlab ="", sub ="", ylab ="")
plot(hclust (data.dist , method ="single"), labels =nci.labs ,
       main="Single Linkage", xlab="", sub ="", ylab ="")

hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out,4)
table(hc.clusters, nci.labs)

par(mfrow = c(1,1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = 'red')

hc.out


set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)


hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels = nci.labs, main = 'Hier Cluster on First Five Score Vectors')
table(cutree(hc.out,4), nci.labs)
