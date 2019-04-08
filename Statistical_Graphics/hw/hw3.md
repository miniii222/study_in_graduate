HW3
================
Min
2019년 3월 22일

``` r
setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Statistical_Graphics/hw/hw3")
library(readr); library(tidyverse); library(gridExtra)
```

    ## -- Attaching packages ---------------------------- tidyverse 1.2.1 --

    ## √ ggplot2 3.1.0     √ purrr   0.3.0
    ## √ tibble  2.0.1     √ dplyr   0.7.8
    ## √ tidyr   0.8.2     √ stringr 1.4.0
    ## √ ggplot2 3.1.0     √ forcats 0.4.0

    ## -- Conflicts ------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## Warning: package 'gridExtra' was built under R version 3.5.3

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
data <- read_csv("HW3data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   ID = col_character(),
    ##   Group = col_character(),
    ##   `Var_A1_V1(-2wk)` = col_double(),
    ##   `Var_A1_V2(0wk)` = col_double(),
    ##   `Var_A1_V4(8wk)` = col_double(),
    ##   `Var_A1_tread_after_V1(-2wk)` = col_double(),
    ##   `Var_A1_tread_after_V2(0wk)` = col_double(),
    ##   `Var_A1_tread_after_V4(8wk)` = col_double(),
    ##   `002_A2_V1(-2wk)` = col_double(),
    ##   `002_A2_V2(0wk)` = col_character(),
    ##   `002_A2_V4(8wk)` = col_character(),
    ##   `002_A2_tread_after_V1(-2wk)` = col_double(),
    ##   `002_A2_tread_after_V2(0wk)` = col_character(),
    ##   `002_A2_tread_after_V4(8wk)` = col_character(),
    ##   `Var_A3_V2(0wk)` = col_double(),
    ##   `Var_A3_V4(8wk)` = col_double(),
    ##   `Var_A3_tread_after_V2(0wk)` = col_double(),
    ##   `Var_A3_tread_after_V4(8wk)` = col_double()
    ## )

preprocessing
=============

``` r
# <0.500 -> NA
for (col in 3:length(data)) {
  na_ind <- which(data[,col] == '<0.500')
  data[na_ind,col] <- NA

}

#chr columns -> numeric columns
chr_col <- as.numeric(which(sapply(data, mode) == 'character'))
chr_col <- chr_col[3:6]#except id, group
data[,chr_col] <- sapply(data[,chr_col], as.numeric)

# column names
colnames(data) <- c('ID', 'group',
                  'var_A1_2wk','var_A1_0wk','var_A1_8wk',
                  'var_A1_after_2wk','var_A1_after_0wk','var_A1_after_8wk',
                  'var_002_A2_2wk','var_002_A2_0wk','var_002_A2_8wk',
          'var_002_A2_after_2wk','var_002_A2_after_0wk','var_002_A2_after_8wk',
                  'var_A3_0wk','var_A3_8wk',
                  'var_A3_after_0wk','var_A3_after_8wk'
                    )

# NA check
colSums(is.na(data))
```

    ##                   ID                group           var_A1_2wk 
    ##                    0                    0                    0 
    ##           var_A1_0wk           var_A1_8wk     var_A1_after_2wk 
    ##                    0                    4                    0 
    ##     var_A1_after_0wk     var_A1_after_8wk       var_002_A2_2wk 
    ##                    0                    4                    0 
    ##       var_002_A2_0wk       var_002_A2_8wk var_002_A2_after_2wk 
    ##                   39                   25                    0 
    ## var_002_A2_after_0wk var_002_A2_after_8wk           var_A3_0wk 
    ##                   38                   18                    0 
    ##           var_A3_8wk     var_A3_after_0wk     var_A3_after_8wk 
    ##                    0                    0                    0

``` r
# variables type check
summary(data)
```

    ##       ID               group             var_A1_2wk       var_A1_0wk    
    ##  Length:80          Length:80          Min.   : 4.465   Min.   : 7.423  
    ##  Class :character   Class :character   1st Qu.: 6.600   1st Qu.: 9.782  
    ##  Mode  :character   Mode  :character   Median : 7.237   Median :10.837  
    ##                                        Mean   : 7.410   Mean   :10.894  
    ##                                        3rd Qu.: 8.224   3rd Qu.:11.996  
    ##                                        Max.   :10.423   Max.   :14.220  
    ##                                                                         
    ##    var_A1_8wk    var_A1_after_2wk var_A1_after_0wk var_A1_after_8wk
    ##  Min.   :12.60   Min.   : 5.459   Min.   : 9.848   Min.   :13.82   
    ##  1st Qu.:14.78   1st Qu.: 7.642   1st Qu.:12.183   1st Qu.:15.96   
    ##  Median :15.40   Median : 8.381   Median :12.979   Median :16.90   
    ##  Mean   :15.52   Mean   : 8.413   Mean   :13.141   Mean   :17.05   
    ##  3rd Qu.:16.37   3rd Qu.: 9.295   3rd Qu.:14.179   3rd Qu.:18.15   
    ##  Max.   :18.26   Max.   :11.167   Max.   :16.067   Max.   :21.01   
    ##  NA's   :4                                         NA's   :4       
    ##  var_002_A2_2wk     var_002_A2_0wk    var_002_A2_8wk   
    ##  Min.   :-0.99116   Min.   :-0.1268   Min.   :-0.7996  
    ##  1st Qu.:-0.08163   1st Qu.: 0.9572   1st Qu.: 0.2910  
    ##  Median : 0.52724   Median : 1.2672   Median : 1.5270  
    ##  Mean   : 0.50297   Mean   : 1.2727   Mean   : 1.2659  
    ##  3rd Qu.: 1.08943   3rd Qu.: 1.6368   3rd Qu.: 2.1636  
    ##  Max.   : 1.74014   Max.   : 2.3919   Max.   : 2.9038  
    ##                     NA's   :39        NA's   :25       
    ##  var_002_A2_after_2wk var_002_A2_after_0wk var_002_A2_after_8wk
    ##  Min.   :-0.2942      Min.   :0.5431       Min.   :-0.2505     
    ##  1st Qu.: 1.1312      1st Qu.:1.4152       1st Qu.: 0.6221     
    ##  Median : 1.5351      Median :1.8172       Median : 1.3186     
    ##  Mean   : 1.5479      Mean   :1.7672       Mean   : 1.2595     
    ##  3rd Qu.: 1.9454      3rd Qu.:2.2579       3rd Qu.: 1.7025     
    ##  Max.   : 3.7340      Max.   :2.6490       Max.   : 2.6922     
    ##                       NA's   :38           NA's   :18          
    ##    var_A3_0wk      var_A3_8wk    var_A3_after_0wk var_A3_after_8wk
    ##  Min.   : 8600   Min.   :25209   Min.   :40192    Min.   : 70525  
    ##  1st Qu.:24399   1st Qu.:49919   1st Qu.:62879    1st Qu.: 98529  
    ##  Median :33686   Median :56707   Median :71799    Median :105581  
    ##  Mean   :32729   Mean   :57141   Mean   :70412    Mean   :105958  
    ##  3rd Qu.:40176   3rd Qu.:65232   3rd Qu.:77409    3rd Qu.:113249  
    ##  Max.   :51286   Max.   :81252   Max.   :97113    Max.   :131446  
    ## 

기본운동/고강도 운동 그룹의 차이
================================

### before 2wk

``` r
before_2wk <- data %>% select(ID,group, ends_with('2wk'))
before_ex <- before_2wk %>%
  gather(var,var_A1_2wk,var_002_A2_2wk,value = 'value') %>% 
  ggplot() + geom_boxplot(aes(var, value, fill = group)) + 
  ggtitle('연구 참가 2주전 운동 전 측정 결과')+
  theme(legend.position = c(0.8, 0.2))

after_ex <- before_2wk %>% gather(var,var_A1_after_2wk,var_002_A2_after_2wk,
                                  value = 'value') %>% ggplot() +     geom_boxplot(aes(var, value, fill = group))+ 
  ggtitle('연구 참가 2주전 운동 후 측정 결과') +
  theme(legend.position = c(0.8, 0.2))

grid.arrange(before_ex, after_ex, ncol = 2)
```

![](hw3_files/figure-markdown_github/unnamed-chunk-3-1.png)

연구에 참가하기 전, 사람들의 본래의 운동능력 비교해보았다. 애초에 Placebo group(기본 운동을 하는 그룹), test group(고강도 운동을 하는 그룹)의 사람들의 운동 능력이 차이가 난다. 연구를 하기전부터 test그룹의 운동 능력이 더 좋은 것으로 나타난다.

### study 0wk

``` r
study_0wk <- data %>% select(ID,group, ends_with('0wk'))

study_0wk %>% select(2,3,5,7 ) %>% 
  gather(var,var_A1_0wk,var_002_A2_0wk,var_A3_0wk,value = 'value') %>%
  ggplot() + geom_boxplot(aes(group, value, fill = group), na.rm=T) + 
  facet_wrap(.~var, scales = 'free')+
  ggtitle('연구 시작 0wk 운동 전 측정 결과')+
  theme(legend.position = 'top')
```

![](hw3_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
study_0wk %>% select(2,4,6,8 ) %>% 
  gather(var, var_A1_after_0wk, var_002_A2_after_0wk,
         var_A3_after_0wk, value = 'value') %>%
  ggplot() + geom_boxplot(aes(group, value, fill = group), na.rm=T) + 
  facet_wrap(.~var, scales = 'free')+
  ggtitle('연구 시작 0wk 운동 후 측정 결과')+
  theme(legend.position = 'top')
```

![](hw3_files/figure-markdown_github/unnamed-chunk-4-2.png)

연구를 시작했을 때도 운동 전, 후 모두 test그룹의 운동능력이 평균적으로 좋은 것으로 보인다.

### study 8wk

``` r
study_8wk <- data %>% select(ID,group, ends_with('8wk'))

study_8wk %>% select(2,3,5,7 ) %>% 
  gather(var,var_A1_8wk,var_002_A2_8wk,var_A3_8wk,value = 'value') %>%
  ggplot() + geom_boxplot(aes(group, value, fill = group), na.rm=T) + 
  facet_wrap(.~var, scales = 'free')+
  ggtitle('연구 시작 8wk 운동 전 측정 결과')+
  theme(legend.position = 'top')
```

![](hw3_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
study_8wk %>% select(2,4,6,8 ) %>% 
  gather(var, var_A1_after_8wk, var_002_A2_after_8wk,
         var_A3_after_8wk, value = 'value') %>%
  ggplot() + geom_boxplot(aes(group, value, fill = group), na.rm=T) + 
  facet_wrap(.~var, scales = 'free')+
  ggtitle('연구 시작 8wk 운동 후 측정 결과')+
  theme(legend.position = 'top')
```

![](hw3_files/figure-markdown_github/unnamed-chunk-5-2.png)

연구 시작 8주 후도 운동 전, 후 모두 test그룹의 운동능력이 평균적으로 좋은 것으로 보인다.

1시간 운동 전/후의 차이
=======================

``` r
data_diff <- data %>%
  mutate( 'var_A1_0wk_diff' = var_A1_after_0wk - var_A1_0wk,
          'var_A1_2wk_diff' = var_A1_after_2wk - var_A1_2wk,
          'var_A1_8wk_diff' = var_A1_after_8wk - var_A1_8wk,
                
      'var_002_A2_0wk_diff' = var_002_A2_after_0wk - var_002_A2_0wk,
      'var_002_A2_2wk_diff' = var_002_A2_after_2wk - var_002_A2_2wk,
      'var_002_A2_8wk_diff' = var_002_A2_after_8wk - var_002_A2_8wk,
                
      'var_A3_0wk_diff' = var_A3_after_0wk - var_A3_0wk,
      'var_A3_8wk_diff' = var_A3_after_8wk - var_A3_8wk) %>% 
  
  select(group, ends_with('diff')) %>% 
  gather(var, var_A1_0wk_diff,var_A1_2wk_diff,var_A1_8wk_diff,
        var_002_A2_0wk_diff,var_002_A2_2wk_diff,var_002_A2_8wk_diff,
        var_A3_0wk_diff,var_A3_8wk_diff,value = 'value') %>%
  mutate('A' = rep(c('A1','A2','A3'), c(240,240,160)))
```

### 연구 주차에 상관없이 var별 운동 전/후 차이 비교

``` r
ggplot(data_diff) +
  geom_boxplot(aes(group, value, fill = group), na.rm=T) + 
  facet_wrap(A~., scales = 'free')
```

![](hw3_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
#변수별 NA개수
data_diff[!complete.cases(data_diff),'A'] %>% table()
```

    ## .
    ## A1 A2 
    ##  4 76

연구 주차에 상관없이 살펴보면, A2변수는 고강도 운동을 했을 때의 증가폭이 기본 운동을 했을 때의 증가폭보다 평균적으로 적은 것을 알 수 있다.하지만, 주의해야할 점은 A2의 변수는 76개의 NA가 포함되어 있어 계산이 불가능한 케이스가 많다는 점이다.

### 연구 주차에 따른 var별 운동 전/후 차이 비교

``` r
data_diff[grepl('2wk', data_diff$var),] %>% ggplot()+
  geom_boxplot(aes(group, value, fill = group), na.rm=T) + 
  facet_wrap(.~A, scales = 'free')+
  ggtitle('before 2wk diff')
```

![](hw3_files/figure-markdown_github/unnamed-chunk-8-1.png)

연구 시작 2주전의 운동 전/후 A1, A2증가량을 비교한 그래프이다. A1, A2 모두 평균적으로 증가하였으나, 증가폭이 placebo group과 비슷하거나, 작다.

``` r
data_diff[grepl('0wk', data_diff$var),] %>% ggplot()+
  geom_boxplot(aes(group, value, fill = group), na.rm=T) + 
  facet_wrap(.~A, scales = 'free')+
  ggtitle('0wk diff')
```

![](hw3_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
data_diff[grepl('0wk', data_diff$var) & is.na(data_diff$value),]
```

    ## # A tibble: 45 x 4
    ##    group   var                 value A    
    ##    <chr>   <chr>               <dbl> <chr>
    ##  1 Test    var_002_A2_0wk_diff    NA A2   
    ##  2 Test    var_002_A2_0wk_diff    NA A2   
    ##  3 Placebo var_002_A2_0wk_diff    NA A2   
    ##  4 Test    var_002_A2_0wk_diff    NA A2   
    ##  5 Test    var_002_A2_0wk_diff    NA A2   
    ##  6 Placebo var_002_A2_0wk_diff    NA A2   
    ##  7 Placebo var_002_A2_0wk_diff    NA A2   
    ##  8 Placebo var_002_A2_0wk_diff    NA A2   
    ##  9 Placebo var_002_A2_0wk_diff    NA A2   
    ## 10 Placebo var_002_A2_0wk_diff    NA A2   
    ## # ... with 35 more rows

연구 시작주의 운동 전/후 A1, A2,A3 증가량을 비교한 그래프이다. A1, A2, A3 모두 평균적으로 증가하였으나, 증가폭이 placebo group과 비슷하거나 작다. 하지만, 0wk A2의 경우 45개의 NA를 포함하고 있어 유의해야 한다.

``` r
data_diff[grepl('8wk', data_diff$var),] %>% ggplot()+
  geom_boxplot(aes(group, value, fill = group), na.rm=T) + 
  facet_wrap(.~A, scales = 'free')+
  ggtitle('after 8wk diff')
```

![](hw3_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
data_diff[grepl('8wk', data_diff$var) & is.na(data_diff$value),]
```

    ## # A tibble: 35 x 4
    ##    group   var                 value A    
    ##    <chr>   <chr>               <dbl> <chr>
    ##  1 Placebo var_A1_8wk_diff        NA A1   
    ##  2 Placebo var_A1_8wk_diff        NA A1   
    ##  3 Test    var_A1_8wk_diff        NA A1   
    ##  4 Placebo var_A1_8wk_diff        NA A1   
    ##  5 Placebo var_002_A2_8wk_diff    NA A2   
    ##  6 Placebo var_002_A2_8wk_diff    NA A2   
    ##  7 Test    var_002_A2_8wk_diff    NA A2   
    ##  8 Placebo var_002_A2_8wk_diff    NA A2   
    ##  9 Placebo var_002_A2_8wk_diff    NA A2   
    ## 10 Placebo var_002_A2_8wk_diff    NA A2   
    ## # ... with 25 more rows

연구 시작 8주가 지났을 때, 운동 전/후 A1, A2,A3 증가량을 비교한 그래프이다. 위 두 기간의 그래프보다는 확실히 test group과 placebo그룹의 차이가 크다. A1,A3는 평균적으로 Placebo그룹보다 증가량이 큰 것을 확인할 수 있다. 하지만, A2는 평균적으로 증가량이 감소하였다. 이 경우에도 A2가 35개의 NA를 포함하고 있어 유의해야 한다.
