setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Datamining/lab")

###1.Basic Commands###
x <- c(1,3,2,5)
x

x = c(1,6,2)
y <- c(1,4,3)
length(x); length(y)
x+y

#a list of all of the objects
ls()
#delete
rm(x,y)
ls()

#delete all
rm(list = ls())

#matrix
x <- matrix(data = 1:4, nrow = 2)
x

matrix(data = 1:4, 2,2, byrow = T)

sqrt(x)
x^2

x<- rnorm(50)
y <- x+rnorm(50, 50, sd = 0.1)

cor(x,y)

set.seed(1303)
rnorm(50)

set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

###2.Graphics###
x<- rnorm(100); y <- rnorm(100)
plot(x,y)
plot(x,y, xlab = 'this is the x-axis', ylab = 'this is the y-axis',
     main = 'Plot of X vs Y')

#pdf('Figure.pdf')
plot(x,y, col = 'green')
#dev.off()

x <- seq(1,10); x
x <- 1:10; x

x <- seq(-pi, pi, length = 50);x
y<-x
#outer : x,y outer product; 간단하게 함수 계산
f <- outer(x,y, function(x,y) cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f, nlevels = 45, add = T)

fa <- (f-t(f))/2
contour(x,y,fa, nlevels = 15)

#image : draw a heatmap
image(x,y,fa)
#persp : 3d plot
##theta, phi : control the angles at which the plot is viewed
persp(x,y,fa)
persp(x,y,fa, theta = 30)
persp(x,y,fa, theta = 30, phi = 20)
persp(x,y,fa, theta = 30, phi = 70)
persp(x,y,fa, theta = 30, phi = 40)

###3.Indexing Data###
A <- matrix(1:16, 4,4); A
A[2,3]

A[c(1,3), c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]

#except
A[-c(1,3),]
A[-c(1,3), -c(1,3,4)]
dim(A)

###4.Loading Data####
Auto = read.table('http://www-bcf.usc.edu/~gareth/ISL/Auto.data',
                  header = T)

#fix : view data in a spreadsheet like window
#편집도 가능!
fix(Auto)

dim(Auto)
Auto <- na.omit(Auto)
dim(Auto)

names(Auto)

###5.Additional Graphical and Numerical Summaries####
plot(cylinders, mpg)

attach(Auto)
plot(cylinders, mpg)

cylinders <- as.factor(cylinders)

#auto draw boxplots
plot(cylinders, mpg)
plot(cylinders, mpg, col = 'red')
plot(cylinders, mpg, col = 'red', varwidth = T)
plot(cylinders, mpg, col = 'red', varwidth = T, horizontal = T)
plot(cylinders, mpg, col = 'red', varwidth = T,
     xlab = 'cylinders', ylab = 'MPG')

hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)

#paris : a scatterplot matrix for every pair of variables
pairs(Auto)
pairs(~mpg + displacement + horsepower + weight +
        acceleration, Auto)
plot(horsepower, mpg)
#identify() : plot 위의 점들이 어떤 점인지 알려줌(name)
identify(horsepower, mpg, name)

summary(Auto)
summary(mpg)
