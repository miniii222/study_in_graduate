setwd("C:/Users/wjssm/Desktop/ace")
library(readxl); library(dplyr); library(tidyr)
student <- read_excel("data.xlsx", sheet = "student_info")
data_B <-read_excel("data.xlsx", sheet = "Q_B")

dt2 <- cbind(student, data_B)
head(dt2)

#가. 교양교육 전반적 도움 정도
#[그림 10]
#mean
mean(dt2$B2_Q1, na.rm=T)
dt2_1 %>% group_by(grade) %>% summarise(mean_B2 = mean(B2_Q1, na.rm=T))
dt2_1 %>% group_by(university) %>% summarise(mean_B2 = mean(B2_Q1, na.rm=T)) %>% 
  arrange(desc(mean_B2))

#[표 14]

#total
n = sum(complete.cases(dt2))
dt2_1 = dt2[complete.cases(dt2),]
round(dt2_1[dt2_1$B2_Q1>=75,'B2_Q1'] %>% length()/n*100, 1)
round(dt2_1[dt2_1$B2_Q1==50 ,'B2_Q1'] %>% length()/n*100, 1)
round(dt2_1[dt2_1$B2_Q1<=25 ,'B2_Q1'] %>% length()/n*100, 1)

n_1 = dt2_1 %>% filter(grade =='1학년(1~2학기)') %>% nrow() #68
n_2 = dt2_1 %>% filter(grade =='2학년(3-4학기)') %>% nrow() #105
n_3 = dt2_1 %>% filter(grade =='3학년(5~6학기)') %>% nrow() #128
n_4 = dt2_1 %>% filter(grade =='4학년(7~8학기)') %>% nrow() #90

#by grade
grade_function<-function(mygrade, myn){
  
  
  helpful = dt2_1 %>% filter(B2_Q1>=75, grade==mygrade) %>% group_by(grade) %>%
    summarise(helpful = n()/myn*100) %>% pull(helpful)
  soso = dt2_1 %>% filter(B2_Q1==50, grade==mygrade) %>% group_by(grade) %>%
    summarise(helpful =n()/myn*100)%>% pull(helpful)
  not = dt2_1 %>% filter(B2_Q1<=25, grade==mygrade) %>% group_by(grade) %>%
    summarise(helpful = n()/myn*100)%>% pull(helpful)
  g100 = dt2_1 %>% filter(grade==mygrade) %>% group_by(grade) %>%
    summarise(helpful = mean(B2_Q1))%>% pull(helpful)
  
  round(c(helpful, soso, not, g100), 1)
}

grade_function('1학년(1~2학기)', n_1)
grade_function('2학년(3-4학기)', n_2)
grade_function('3학년(5~6학기)', n_3)
grade_function('4학년(7~8학기)', n_4)

#by college
college_table <- table(dt2_1$university)

college_function<-function(mycollege, myn){
  
  helpful = dt2_1 %>% filter(B2_Q1>=75, university==mycollege) %>% group_by(university) %>%
    summarise(helpful = n()/myn*100) %>% pull(helpful)
  soso = (dt2_1 %>% filter(B2_Q1==50, university==mycollege) %>% group_by(university) %>%
            summarise(helpful = n()/myn*100))%>% pull(helpful)
  not = (dt2_1 %>% filter(B2_Q1<=25, university==mycollege) %>% group_by(university) %>%
           summarise(helpful = n()/myn*100))%>% pull(helpful)
  g100 = (dt2_1 %>% filter(university==mycollege, !is.na(B2_Q1)) %>% group_by(university) %>%
            summarise(helpful = mean(B2_Q1)))%>% pull(helpful)
  
  round(c(1, helpful,2, soso, 3,not,4, g100), 1)
}

for(i in 1: length(college_table)){
print(names(college_table[i]))
print(college_function(names(college_table[i]), as.vector(college_table[i])))
}

####
dt2_2 <- dt2_1[,-20]

dt2_2 %>% gather(question, starts_with('B1_'))
