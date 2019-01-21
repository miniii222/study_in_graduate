setwd("C:/Users/wjssm/Desktop/ace")
library(readxl); library(dplyr); library(tidyr)
student <- read_excel("data.xlsx", sheet = "student_info")
data_B <-read_excel("data.xlsx", sheet = "Q_B")

dt2 <- cbind(student, data_B)
head(dt2)

#가. 교양교육 전반적 도움 정도
#[그림 10]
#mean
n = sum(complete.cases(dt2))
dt2_1 = dt2[complete.cases(dt2),]
mean(dt2$B2_Q1, na.rm=T)
dt2_1 %>% group_by(grade) %>% summarise(mean_B2 = mean(B2_Q1, na.rm=T))
dt2_1 %>% group_by(university) %>% summarise(mean_B2 = mean(B2_Q1, na.rm=T)) %>% 
  arrange(desc(mean_B2))

#[표 14]

#total

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

for(i in 6:19) print(dt2_2 %>% select(i) %>% table())

###
dt2_2 %>% group_by(grade) %>%
  summarise(m1 = mean(B1_Q1),m2 = mean(B1_Q2),m3 = mean(B1_Q3),
            m4 = mean(B1_Q4),m5 = mean(B1_Q5),m6 = mean(B1_Q6),
            m7 = mean(B1_Q7),m1 = mean(B1_Q7),m1 = mean(B1_Q7),
            m8 = mean(B1_Q8),m9 = mean(B1_Q9),m10 = mean(B1_Q10),
            m11 = mean(B1_Q11),m12 = mean(B1_Q12),m13 = mean(B1_Q13),
            m14 = mean(B1_Q14)) %>% 
  mutate_if(is.numeric, round, digits=1) %>% View()

###
dt2_2 %>% group_by(university) %>%
  summarise(m1 = mean(B1_Q1),m2 = mean(B1_Q2),m3 = mean(B1_Q3),
            m4 = mean(B1_Q4),m5 = mean(B1_Q5),m6 = mean(B1_Q6),
            m7 = mean(B1_Q7),m1 = mean(B1_Q7),m1 = mean(B1_Q7),
            m8 = mean(B1_Q8),m9 = mean(B1_Q9),m10 = mean(B1_Q10),
            m11 = mean(B1_Q11),m12 = mean(B1_Q12),m13 = mean(B1_Q13),
            m14 = mean(B1_Q14), 
            mm = mean(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)) %>% 
  mutate_if(is.numeric, round, digits=1) %>% select(university, mm) %>% 
  arrange(desc(mm))

##공감 data
data_c <-read_excel("data.xlsx", sheet = "Q_C")
dt3 <- cbind(student, data_c)
dt3<-dt3[complete.cases(dt3),]

head(dt3)
mean(dt3$Question)
dt3 %>% group_by(grade) %>% summarise(mean = mean(Question))
dt3 %>% group_by(university) %>% summarise(mean = mean(Question))

dt3 %>% filter(Question>=75) %>% summarise(m = n()/nrow(dt3)*100)
dt3 %>% filter(Question==50) %>% summarise(m = n()/nrow(dt3)*100)
dt3 %>% filter(Question<=25) %>% summarise(m = n()/nrow(dt3)*100)

n_1 = dt3 %>% filter(grade =='1학년(1~2학기)') %>% nrow() #70
n_2 = dt3 %>% filter(grade =='2학년(3-4학기)') %>% nrow() #106
n_3 = dt3 %>% filter(grade =='3학년(5~6학기)') %>% nrow() #135
n_4 = dt3 %>% filter(grade =='4학년(7~8학기)') %>% nrow() #91


#by grade
grade_function<-function(mygrade, myn){
  
  
  helpful = dt3 %>% filter(Question>=75, grade==mygrade) %>% group_by(grade) %>%
    summarise(helpful = n()/myn*100) %>% pull(helpful)
  soso = dt3 %>% filter(Question==50, grade==mygrade) %>% group_by(grade) %>%
    summarise(helpful =n()/myn*100)%>% pull(helpful)
  not = dt3 %>% filter(Question<=25, grade==mygrade) %>% group_by(grade) %>%
    summarise(helpful = n()/myn*100)%>% pull(helpful)
  g100 = dt3 %>% filter(grade==mygrade) %>% group_by(grade) %>%
    summarise(helpful = mean(Question))%>% pull(helpful)
  
  round(c(helpful, soso, not, g100), 1)
}

grade_function('1학년(1~2학기)', n_1)
grade_function('2학년(3-4학기)', n_2)
grade_function('3학년(5~6학기)', n_3)
grade_function('4학년(7~8학기)', n_4)

#by college
college_table <- table(dt3$university)

college_function<-function(mycollege, myn){
  
  helpful = dt3 %>% filter(Question>=75, university==mycollege) %>% group_by(university) %>%
    summarise(helpful = n()/myn*100) %>% pull(helpful)
  soso = (dt3 %>% filter(Question==50, university==mycollege) %>% group_by(university) %>%
            summarise(helpful = n()/myn*100))%>% pull(helpful)
  not = (dt3 %>% filter(Question<=25, university==mycollege) %>% group_by(university) %>%
           summarise(helpful = n()/myn*100))%>% pull(helpful)
  g100 = (dt3 %>% filter(university==mycollege, !is.na(Question)) %>% group_by(university) %>%
            summarise(helpful = mean(Question)))%>% pull(helpful)
  
  round(c(1, helpful,2, soso, 3,not,4, g100), 1)
}

for(i in 1: length(college_table)){
  print(names(college_table[i]))
  print(college_function(names(college_table[i]), as.vector(college_table[i])))
}
