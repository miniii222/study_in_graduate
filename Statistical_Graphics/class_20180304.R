library(tidyverse)
setwd("C:/Users/wjssm/Desktop/0.graduate/3rd/Statistical_Graphics")
mpg

class(iris$Species)
#fator와 chr의 차이
as.numeric(iris$Species)
as.numeric(mpg$manufacturer)

ggplot(mpg) + geom_point(aes(displ, hwy, color = class))

shapes <- tibble(
  shape = c(0,1,2,5,3,4,6:19,22,21,24,23,20),
  x = (0:24 %/% 5)/2,
  y = (-(0:24 %% 5)) /4
)

ggplot(shapes, aes(x,y))+
  geom_point(aes(shape = shape, size = 5, fill = 'red'))+
  geom_text(aes(label = shape), hjust = 0, nudge_x = 0.15)

ggplot(mpg)+
  geom_point(aes(displ, hwy)) +
  facet_wrap(~class, nrow = 2)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(data = filter(mpg, class == 'subcompact'), se = F)
