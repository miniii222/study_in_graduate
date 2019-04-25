HW3
================
2019년 4월 24일

1
=

``` r
setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Advanced_linear_models")


ratdata<-read.csv("ratdata.csv",header=TRUE)
attach(ratdata)

y<-response
time<-log(1+(age-45)/10)

N<-length(unique(rat))
```

``` r
library(lme4)
```

    ## Warning: package 'lme4' was built under R version 3.5.3

    ## Loading required package: Matrix

``` r
ratdata<-cbind(ratdata,time)

m3 <- lmer(response ~ 1 + as.factor(treat):time+ (1 + time|rat), ratdata, REML=TRUE)
```

    ## boundary (singular) fit: see ?isSingular

``` r
m4<- lmer(response ~ 1+ as.factor(treat):time+ (1|rat) + (0+time|rat), ratdata, REML=TRUE)
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(m3)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: response ~ 1 + as.factor(treat):time + (1 + time | rat)
    ##    Data: ratdata
    ## 
    ## REML criterion at convergence: 932.4
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.25588 -0.66674 -0.01326  0.58310  2.89227 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr
    ##  rat      (Intercept) 3.429286 1.85183      
    ##           time        0.001079 0.03285  1.00
    ##  Residual             1.444606 1.20192      
    ## Number of obs: 252, groups:  rat, 50
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)               68.6061     0.3272  209.70
    ## as.factor(treat)con:time   7.3178     0.2836   25.80
    ## as.factor(treat)hig:time   6.8750     0.2301   29.88
    ## as.factor(treat)low:time   7.5045     0.2274   32.99
    ## 
    ## Correlation of Fixed Effects:
    ##                (Intr) as.fctr(trt)c: as.fctr(trt)h:
    ## as.fctr(trt)c: -0.316                              
    ## as.fctr(trt)h: -0.325  0.103                       
    ## as.fctr(trt)l: -0.336  0.106          0.109        
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
summary(m4)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: response ~ 1 + as.factor(treat):time + (1 | rat) + (0 + time |  
    ##     rat)
    ##    Data: ratdata
    ## 
    ## REML criterion at convergence: 932.4
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.25575 -0.65899 -0.01164  0.58358  2.88309 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  rat      (Intercept) 3.565    1.888   
    ##  rat.1    time        0.000    0.000   
    ##  Residual             1.445    1.202   
    ## Number of obs: 252, groups:  rat, 50
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)               68.6074     0.3312  207.13
    ## as.factor(treat)con:time   7.3139     0.2808   26.05
    ## as.factor(treat)hig:time   6.8711     0.2276   30.19
    ## as.factor(treat)low:time   7.5069     0.2252   33.34
    ## 
    ## Correlation of Fixed Effects:
    ##                (Intr) as.fctr(trt)c: as.fctr(trt)h:
    ## as.fctr(trt)c: -0.327                              
    ## as.fctr(trt)h: -0.340  0.111                       
    ## as.fctr(trt)l: -0.351  0.115          0.119        
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

### AIC & SBC

``` r
l1 <- lmer(response ~ 1 + as.factor(treat):time+ (1 |rat), ratdata, REML=F)
extractAIC(l1)
```

    ## [1]   6.0000 940.6526

``` r
l2 <- lmer(response ~ 1 + time+ (1 |rat), ratdata, REML=F)
extractAIC(l2)
```

    ## [1]   4.0000 941.2445

``` r
df <- data.frame(row.names = c('Separate_average_slopes','common_average_slopes'),
                 parameter = c(6,4),
                 AIC = c(extractAIC(l1)[2]/(-2),extractAIC(l2)[2]/(-2)),
                 BIC = c(BIC(l1)/(-2),BIC = c(BIC(l2)/(-2))
                 ))

df
```

    ##                         parameter       AIC       BIC
    ## Separate_average_slopes         6 -470.3263 -480.9146
    ## common_average_slopes           4 -470.6223 -477.6811
