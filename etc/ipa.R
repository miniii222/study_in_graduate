setwd("C:/Users/wjssm/Desktop/ace")
data <- read_excel("data.xlsx", sheet = "Q_A")

#delete NAs
data <- data[complete.cases(data), ]



c1 <- c() #correlation variable
m1 <- c() #mean variable

for (i in 1:10){
  c1 <-c(c1, cor(data[,i], data$performance) )
}

m1 <- c(mean(data$A1_Q1), mean(data$A1_Q2),mean(data$A1_Q3),mean(data$A1_Q4),
        mean(data$A1_Q5),mean(data$A1_Q6),mean(data$A1_Q7),mean(data$A1_Q8),
        mean(data$A1_Q9),mean(data$A1_Q10))

dt <- data.frame(imp = c1, perf = m1)

#scale
dt = transform(dt, imp_sc = scale(imp), perf_sc = scale(perf))
dt

#IPA plot
library(ggplot2)
ggplot(dt)+geom_point(aes(imp_sc, perf_sc))
ggplot(dt)+geom_point(aes(imp, perf))

mean(dt$imp)
mean(dt$perf)

#http://datatales.co.kr/entry/4-R%EC%9D%84-%EC%9D%B4%EC%9A%A9%ED%95%9C-IPA-%EC%82%B0%EC%A0%90%EB%8F%84-%EC%9E%91%EC%84%B1