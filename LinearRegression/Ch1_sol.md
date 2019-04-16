CH1\_sol
================
SeungMin
2019년 4월 16일

1.21(a)
=======

``` r
x <- c(1,0,2,0,3,1,0,1,2,0)
y <- c(16,9,17,12,22,13,8,15,19,11)

lm <- lm(y~x)
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##   -2.2   -1.2    0.3    0.8    1.8 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  10.2000     0.6633  15.377 3.18e-07 ***
    ## x             4.0000     0.4690   8.528 2.75e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.483 on 8 degrees of freedom
    ## Multiple R-squared:  0.9009, Adjusted R-squared:  0.8885 
    ## F-statistic: 72.73 on 1 and 8 DF,  p-value: 2.749e-05

``` r
plot(x,y)
abline(lm)
```

![](Ch1_sol_files/figure-markdown_github/unnamed-chunk-1-1.png)

A linear regression function appears to give a good fit.

1.42(d)
=======

``` r
x <- c(7,12,4,14,25,30)
y <- c(128,213,75,250,446,540)
beta <- seq(17,19, by = 0.01)

n <- 6; sigma_2 <- 16
lik <- c()

lik_fun <- function(beta){
  (1/(sqrt(2*pi*sigma_2)))^n * exp(-1/(2*sigma_2)*sum((y - beta*x)^2))}

for(i in 1:length(beta)){
  
  lik[i] <- lik_fun(beta[i])
}

plot(beta, lik, 'l')
```

![](Ch1_sol_files/figure-markdown_github/unnamed-chunk-2-1.png)
