#PCR and PLS Regression

library(ISLR)
Hitters <- na.omit(Hitters)

###1.Principal Components Regression###
library(pls)
set.seed(2)

pcr.fit <- pcr(Salary~., data = Hitters, scale = T, validation = 'CV')
summary(pcr.fit)

#cv MSE
validationplot(pcr.fit, val.type = 'MSEP')

set.seed(1)
train <- sample(nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

set.seed(1)
pcr.fit <- pcr(Salary~., data = Hitters, subset = train,
               scale = T, validation = "CV")
validationplot(pcr.fit, val.type = 'MSEP')
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred - y.test)^2)

pcr.fit <- pcr(y~x, scale = T, ncomp = 7)
summary(pcr.fit)


###2.Partial Least Squares###
set.seed(1)
pls.fit <- plsr(Salary~., data = Hitters, subset = train,
                scale = T, validation = 'CV')
summary(pls.fit)
###M=2 is the best

validationplot(pls.fit, val.type = 'MSEP')

pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2)


pls.fit <- plsr(Salary~., data = Hitters, scale = T, ncomp = 2)
summary(pls.fit)
