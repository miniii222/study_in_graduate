setwd("C:/Users/wjssm/Desktop/ace")
library(readxl); library(dplyr)
student <- read_excel("data.xlsx", sheet = "student_info")
data_B <-read_excel("data.xlsx", sheet = "Q_B")

dt2 <- cbind(student, data_B)
head(dt2)

#가. 교양교육 전반적 도움 정도
#mean
mean(dt2$B2_Q1, na.rm=T)
dt2 %>% group_by(grade) %>% summarise(mean_B2 = mean(B2_Q1, na.rm=T))
dt2 %>% group_by(university) %>% summarise(mean_B2 = mean(B2_Q1, na.rm=T)) %>% 
  arrange(desc(mean_B2))

dt2[dt2$B2_Q1>=75 & !is.na(dt2$B2_Q1),'B2_Q1'] %>% length()/n

n<-nrow(student)
