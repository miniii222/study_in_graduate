hw1
================
SeungMin
2019년 3월 7일

``` r
setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Statistical_Graphics/hw/hw1")
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

1
=

``` r
ggplot(mpg) + geom_point(aes(cyl, hwy))
```

![](hw1_files/figure-markdown_github/unnamed-chunk-2-1.png)

2
=

``` r
ggplot(mpg) + geom_point(aes(drv, class))
```

![](hw1_files/figure-markdown_github/unnamed-chunk-3-1.png)

drv, class 모두 categorical 변수이기 때문에

3
=

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
```

![](hw1_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(mpg) + geom_point(aes(displ, hwy),color = 'blue')
```

![](hw1_files/figure-markdown_github/unnamed-chunk-4-2.png)

mapping 안에 blue를 써서 안 바뀜. 밖에 써줘야 blue로 색이 바뀜

4
=

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ .)
```

![](hw1_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
ggplot(data = mpg) +
geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(. ~ cyl)
```

![](hw1_files/figure-markdown_github/unnamed-chunk-5-2.png)

facet\_grid(y~x) 이므로, 첫번째 그래프에서 y값만 drv로 지정해주면, y축을 drv의 값에 따라 나눠서 플럿을 그리고, x값은 원래 축인 displ로 그대로이다. 또한, 두번째 그래프에서는 x값만 cyl로 지정해주어, cyl값에 따라 x축을 나누고 y축은 displ그대로이다. 따라서 여기서 '.'은 원래 축 그대로 두라는 의미를 갖는다.

5
=

``` r
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](hw1_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggplot() + geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](hw1_files/figure-markdown_github/unnamed-chunk-6-2.png)

다르지 않다. 같다. ggplot(data = , aes(x = , y = )) 에 미리 지정해주면 뒤에 geom\_ 함수에 다 영향을 미치므로. 한 번만 선언해줘도 매번 선언해주는 것과 같은 결과.

6
=

``` r
ggplot(mpg, aes(displ, hwy)) + geom_point() +
  geom_smooth(se = F, aes(group = drv))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](hw1_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
ggplot(mpg, aes(displ,hwy))  + geom_point(size = 4, color = 'white')+ geom_point(aes(color = drv))
```

![](hw1_files/figure-markdown_github/unnamed-chunk-7-2.png)
