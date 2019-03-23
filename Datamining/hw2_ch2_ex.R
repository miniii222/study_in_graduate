setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Datamining/hw")

#Exercise 2.9
Auto = read.table('http://www-bcf.usc.edu/~gareth/ISL/Auto.data',
                  header = T)

#'?' -> NA
for (i in 1:ncol(Auto)) Auto[Auto[,i] == '?', i] <- NA

colSums(is.na(Auto))
dim(Auto)
#na.omit
Auto <- na.omit(Auto); dim(Auto)
Auto$horsepower <- as.integer(Auto$horsepower)
##(a)
summary(Auto)


# qualitative : name, origin(1.American 2.European 3.Japanese)
# quantitaive : mpg, cylinders, displacement, horsepower,
#               weight, acceleration, year


##(b)
attach(Auto)
quant_col <- c('mpg', 'cylinders', 'displacement', 'horsepower',
              'weight', 'acceleration', 'year')

sapply(Auto[,quant_col], range)

##(c)
sapply(Auto[,quant_col], mean)
round(sapply(Auto[,quant_col], mean),2)
sapply(Auto[,quant_col], sd)
round(sapply(Auto[,quant_col], sd),2)
##(d)
Auto2 <- Auto[-c(10:85),]; dim(Auto2)
sapply(Auto2[,quant_col], range)

sapply(Auto2[,quant_col], mean)
round(sapply(Auto2[,quant_col], mean),2)

sapply(Auto2[,quant_col], sd)
round(sapply(Auto2[,quant_col], sd),2)
##(e)
pairs(Auto[, quant_col])

##(f)
plot(cylinders, mpg)
plot(displacement, mpg)
plot(horsepower, mpg)
plot(weight, mpg)
plot(acceleration, mpg)
plot(year, mpg)



#Exercise 2.10
##(a)
library(MASS)
dim(Boston)
?Boston

##(b)
pairs(Boston)

##(c)
Boston_col <- colnames(Boston)
par(mfrow = c(2,2))

for(i in 2:14){
  
  x <- Boston[,i]
  plot(x, Boston$crim, main = paste(Boston_col[i], '& crim'),
       ylab = 'crim rate',
       xlab= paste(Boston_col[i]))
}

##(d)
par(mfrow = c(1,1))
plot(Boston$crim)
plot(Boston$tax)
plot(Boston$ptratio)

##(e)
sum(Boston$chas == 1)

##(f)
median(Boston$ptratio)

##(g)
Boston[Boston$medv ==min(Boston$medv),]
sapply(Boston, mean)
##(h)
nrow(Boston[Boston$rm > 7, ])
nrow(Boston[Boston$rm > 8, ])
