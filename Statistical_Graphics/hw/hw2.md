hw2
================
Min
2019년 3월 14일

``` r
setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Statistical_Graphics/hw/hw2")
library(tidyverse)
```

    ## -- Attaching packages ---------------------------- tidyverse 1.2.1 --

    ## √ ggplot2 3.1.0     √ purrr   0.3.0
    ## √ tibble  2.0.1     √ dplyr   0.7.8
    ## √ tidyr   0.8.2     √ stringr 1.4.0
    ## √ readr   1.3.1     √ forcats 0.4.0

    ## -- Conflicts ------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 3.5.3

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
diamonds
```

    ## # A tibble: 53,940 x 10
    ##    carat cut       color clarity depth table price     x     y     z
    ##    <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
    ##  1 0.23  Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
    ##  2 0.21  Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
    ##  3 0.23  Good      E     VS1      56.9    65   327  4.05  4.07  2.31
    ##  4 0.290 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
    ##  5 0.31  Good      J     SI2      63.3    58   335  4.34  4.35  2.75
    ##  6 0.24  Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48
    ##  7 0.24  Very Good I     VVS1     62.3    57   336  3.95  3.98  2.47
    ##  8 0.26  Very Good H     SI1      61.9    55   337  4.07  4.11  2.53
    ##  9 0.22  Fair      E     VS2      65.1    61   337  3.87  3.78  2.49
    ## 10 0.23  Very Good H     VS1      59.4    61   338  4     4.05  2.39
    ## # ... with 53,930 more rows

1
=

``` r
summary(diamonds %>% select(8:10))
```

    ##        x                y                z         
    ##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
    ##  1st Qu.: 4.710   1st Qu.: 4.720   1st Qu.: 2.910  
    ##  Median : 5.700   Median : 5.710   Median : 3.530  
    ##  Mean   : 5.731   Mean   : 5.735   Mean   : 3.539  
    ##  3rd Qu.: 6.540   3rd Qu.: 6.540   3rd Qu.: 4.040  
    ##  Max.   :10.740   Max.   :58.900   Max.   :31.800

``` r
#boxplot
p_x <-ggplot(diamonds)+geom_boxplot(aes(1,x))
p_y <-ggplot(diamonds)+geom_boxplot(aes(1,y))
p_z <-ggplot(diamonds)+geom_boxplot(aes(1,z))

grid.arrange(p_x, p_y, p_z, nrow =1)
```

![](hw2_files/figure-markdown_github/unnamed-chunk-2-1.png)

x 는 0~10.74에 분포 / y는 0~58.900에 분포 / z 는 0~31.800 x,y,z모두 outlier가 존재한다.

``` r
p_x <-ggplot(diamonds)+geom_density(aes(x))+xlim(c(0,60))
p_y <-ggplot(diamonds)+geom_density(aes(y))+xlim(c(0,60))
p_z <-ggplot(diamonds)+geom_density(aes(z))+xlim(c(0,60))

grid.arrange(p_x, p_y, p_z, ncol =1)
```

![](hw2_files/figure-markdown_github/unnamed-chunk-3-1.png)

-   세 변수 모두 skewed to right.
-   대부분의 다이아몬드가 작은 값에 모여있다.
-   x,y,z 의 분포 모양이 비슷하다.
-   range가 가장 넓은 y : width / z : depth / x : length

2.
==

``` r
summary(diamonds$price)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     326     950    2401    3933    5324   18823

``` r
ggplot(diamonds) + geom_histogram(aes(x = price),bins = 300)
```

![](hw2_files/figure-markdown_github/unnamed-chunk-4-1.png)

히스토그램의 구간을 촘촘히 해보니까, 중간에 값이 비어있는 구간이 보인다.

``` r
ggplot(filter(diamonds, price<2000, price>1000)) + geom_histogram(aes(x = price),bins = 200)
```

![](hw2_files/figure-markdown_github/unnamed-chunk-5-1.png)

price 1500근처에서 비어있다.

3.
==

``` r
paste0('0.99 carat인 다이아몬드의 개수는 : ',
       diamonds %>% filter(carat == 0.99) %>% nrow())
```

    ## [1] "0.99 carat인 다이아몬드의 개수는 : 23"

``` r
paste0('1 carat인 다이아몬드의 개수는 : ',
       diamonds %>% filter(carat == 1) %>% nrow())
```

    ## [1] "1 carat인 다이아몬드의 개수는 : 1558"

``` r
ggplot(diamonds)+geom_histogram(aes(carat), bins = 150)
```

![](hw2_files/figure-markdown_github/unnamed-chunk-6-1.png)

carat의 전체적인 분포를 살펴보면, 특정 값들에서 유난히 많은 개수를 갖고 있다. 0.99캐럿, 1캐럿 다이아몬드의 개수도 이 사실에 미루어 보았을 때, 특정 값으로 반올림하지 않았을까하는 추측을 해볼 수 있다.

4.
==

``` r
summary(diamonds$carat)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2000  0.4000  0.7000  0.7979  1.0400  5.0100

``` r
hist(diamonds$carat)
```

![](hw2_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
#cut_number
ggplot(data = diamonds) +
  geom_freqpoly(aes(price, color = cut_number(carat,5)))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](hw2_files/figure-markdown_github/unnamed-chunk-7-2.png)

``` r
#cut_width
ggplot(diamonds) +
  geom_freqpoly(aes(price,
                    color = cut_width(carat, 1, boundary = 0)))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](hw2_files/figure-markdown_github/unnamed-chunk-7-3.png)

cut\_number는 구간의 정확한 개수를 지정해줘야하고, cut\_width는 구간의 구간길이를 지정해주면 된다. cut\_number는 구간의 개수를 지정해주기 때문에 구간의 길이가 등간격이 아니지만, 한 구간에 같은 개수의 자료가 들어간다. cut\_width는 구간의 길이를 등간격으로 지정한다.

캐럿별 가격을 살펴보려고 할 때, cut\_number가 좀 더 informative한 것 같다.

5.
==

``` r
ggplot(diamonds) + geom_point(aes(price, carat))
```

![](hw2_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ggplot(diamonds) + geom_boxplot(aes(cut_number(price, 10), carat))
```

![](hw2_files/figure-markdown_github/unnamed-chunk-8-2.png)

그냥 산점도를 그렸을 때도 가격이 오를수록 carat이 커지는 것을 알 수 있었지만, 구간별 boxplot으로 나타냄으로써 더 뚜렷한 경향을 볼 수 있다.

6.
==

``` r
ggplot(diamonds) + 
  geom_boxplot(aes(cut_width(price, 2000), carat)) +
  coord_flip()
```

![](hw2_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ggplot(diamonds, aes(carat, fill = cut_width(price, 2000))) +
  geom_density(alpha = 0.5)
```

![](hw2_files/figure-markdown_github/unnamed-chunk-9-2.png)

가격이 비싼 다이아몬드의 경우, carat의 분포가 넓은 것을 알 수 있다. 평균적으로 보면, 큰 다이아몬드일수록 캐럿이 크지만, 좀더 자세히 살펴보면, 특정 캐럿의 다이아몬드의 가격이 높게 측정된 것을 알 수 있다. 소비자들이 선호하는 특정 캐럿의 다이아몬드가 있는 것이 아닌가 유추해볼 수 있다.

7.
==

### 1)

``` r
tb_cases <- table2 %>% filter(type == 'cases') %>% 
  rename(cases = count) %>% select(-type)
tb_cases
```

    ## # A tibble: 6 x 3
    ##   country      year  cases
    ##   <chr>       <int>  <int>
    ## 1 Afghanistan  1999    745
    ## 2 Afghanistan  2000   2666
    ## 3 Brazil       1999  37737
    ## 4 Brazil       2000  80488
    ## 5 China        1999 212258
    ## 6 China        2000 213766

### 2)

``` r
tb_pop <-table2 %>% filter(type == 'population') %>% 
  rename(population = count) %>% select(-type)
tb_pop
```

    ## # A tibble: 6 x 3
    ##   country      year population
    ##   <chr>       <int>      <int>
    ## 1 Afghanistan  1999   19987071
    ## 2 Afghanistan  2000   20595360
    ## 3 Brazil       1999  172006362
    ## 4 Brazil       2000  174504898
    ## 5 China        1999 1272915272
    ## 6 China        2000 1280428583

### 3)

``` r
tb_case_by_pop <- left_join(tb_cases, tb_pop) %>% 
  mutate(case_by_pop = cases / population * 10000)
```

    ## Joining, by = c("country", "year")

``` r
tb_case_by_pop
```

    ## # A tibble: 6 x 5
    ##   country      year  cases population case_by_pop
    ##   <chr>       <int>  <int>      <int>       <dbl>
    ## 1 Afghanistan  1999    745   19987071       0.373
    ## 2 Afghanistan  2000   2666   20595360       1.29 
    ## 3 Brazil       1999  37737  172006362       2.19 
    ## 4 Brazil       2000  80488  174504898       4.61 
    ## 5 China        1999 212258 1272915272       1.67 
    ## 6 China        2000 213766 1280428583       1.67

### 4)

``` r
bind_rows(table2,
      tb_case_by_pop %>% select(-cases, -population) %>% 
  mutate(type = 'case_by_pop')
  %>% rename(count = case_by_pop)) %>% arrange(country, year)
```

    ## # A tibble: 18 x 4
    ##    country      year type          count
    ##    <chr>       <int> <chr>         <dbl>
    ##  1 Afghanistan  1999 cases       7.45e+2
    ##  2 Afghanistan  1999 population  2.00e+7
    ##  3 Afghanistan  1999 case_by_pop 3.73e-1
    ##  4 Afghanistan  2000 cases       2.67e+3
    ##  5 Afghanistan  2000 population  2.06e+7
    ##  6 Afghanistan  2000 case_by_pop 1.29e+0
    ##  7 Brazil       1999 cases       3.77e+4
    ##  8 Brazil       1999 population  1.72e+8
    ##  9 Brazil       1999 case_by_pop 2.19e+0
    ## 10 Brazil       2000 cases       8.05e+4
    ## 11 Brazil       2000 population  1.75e+8
    ## 12 Brazil       2000 case_by_pop 4.61e+0
    ## 13 China        1999 cases       2.12e+5
    ## 14 China        1999 population  1.27e+9
    ## 15 China        1999 case_by_pop 1.67e+0
    ## 16 China        2000 cases       2.14e+5
    ## 17 China        2000 population  1.28e+9
    ## 18 China        2000 case_by_pop 1.67e+0

8.
==

``` r
stocks <- tibble(
    year = rep(c(2015,2016), each = 2),
    half = c(1,2,1,2),
    return = c(1.88, 0.59, 0.92, 0.17)
)

stocks %>% spread(year, return)
```

    ## # A tibble: 2 x 3
    ##    half `2015` `2016`
    ##   <dbl>  <dbl>  <dbl>
    ## 1     1   1.88   0.92
    ## 2     2   0.59   0.17

``` r
stocks %>% spread(year, return) %>% 
  gather('year', 'return','2015':'2016')
```

    ## # A tibble: 4 x 3
    ##    half year  return
    ##   <dbl> <chr>  <dbl>
    ## 1     1 2015    1.88
    ## 2     2 2015    0.59
    ## 3     1 2016    0.92
    ## 4     2 2016    0.17

spread후 gather를 했을 때, year의 type은 double 이 아닌 chr로 되어 있다. spread하였을 때, year의 값들(2015,2016)이 column name으로 변환되면서, chr 타입으로 바뀌었다. 다시 gather를 통해 불러오는 과정을 통해 chr타입으로 변환되었기 때문이다.

9.
==

``` r
preg <- tribble(
  ~pregnant, ~male, ~female,
  'yes', NA, 10,
  'no', 20, 12
)

preg
```

    ## # A tibble: 2 x 3
    ##   pregnant  male female
    ##   <chr>    <dbl>  <dbl>
    ## 1 yes         NA     10
    ## 2 no          20     12

``` r
preg %>% gather('sex','count','male','female')
```

    ## # A tibble: 4 x 3
    ##   pregnant sex    count
    ##   <chr>    <chr>  <dbl>
    ## 1 yes      male      NA
    ## 2 no       male      20
    ## 3 yes      female    10
    ## 4 no       female    12

'pregnant','sex','count'를 변수로 갖는 tidy data 형태로 만들었다.
