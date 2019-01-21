setwd("C:/Users/wjssm/Desktop/ace")


####IPA plot
#1
data <- read_excel("data.xlsx", sheet = "Q_A")

#delete NAs
data <- data[complete.cases(data), ]


c1 <- c() #correlation variable
m1 <- c() #mean variable

for (i in 1:10){
  c1 <-c(c1, cor(data[,i], data$만족도) )
}

m1 <- c(mean(data$A1_Q1), mean(data$A1_Q2),mean(data$A1_Q3),mean(data$A1_Q4),
        mean(data$A1_Q5),mean(data$A1_Q6),mean(data$A1_Q7),mean(data$A1_Q8),
        mean(data$A1_Q9),mean(data$A1_Q10))

dt <- data.frame(imp = c1, perf = m1)

#scale
dt = transform(dt, imp_sc = scale(imp), perf_sc = scale(perf))
dt$imp <- round(dt$imp*100,1)
dt$perf <- round(dt$perf,1)
dt$label = colnames(data)[1:10]


library(ggplot2)
ggplot(dt)+geom_point(aes(imp_sc, perf_sc))

p<-ggplot(dt, aes(imp, perf,label = colnames(data)[1:10]))+
  geom_point()+theme_bw()+ylim(45,95)+
  geom_hline(yintercept = mean(data$만족도), linetype = 'dashed')+
  geom_vline(xintercept = mean(dt$imp), linetype = 'dashed')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
p+geom_text()


#2
data2 <- read_excel("data.xlsx", sheet = "Q_B")

#delete NAs
data2 <- data2[complete.cases(data2), ]


c1 <- c() #correlation variable
m1 <- c() #mean variable

for (i in 1:14){
  c1 <-c(c1, cor(data2[,i], data2$B2_Q1) )
}

m1 <- c(mean(data2$B1_Q1), mean(data2$B1_Q2),mean(data2$B1_Q3),mean(data2$B1_Q4),
        mean(data2$B1_Q5),mean(data2$B1_Q6),mean(data2$B1_Q7),mean(data2$B1_Q8),
        mean(data2$B1_Q9),mean(data2$B1_Q10),mean(data2$B1_Q11),
        mean(data2$B1_Q12),mean(data2$B1_Q13),mean(data2$B1_Q14)
        )

dtdt <- data.frame(imp = c1, perf = m1)

#scale

dtdt$imp <- round(dtdt$imp*100,1)
dtdt$perf <- round(dtdt$perf,1)
dtdt$label = colnames(data2)[1:14]


q<-ggplot(dtdt, aes(imp, perf,label = colnames(data2)[1:14]))+
  geom_point()+theme_bw()+ylim(45,95)+
  geom_hline(yintercept = mean(data2$B2_Q1), linetype = 'dashed')+
  geom_vline(xintercept = mean(dtdt$imp), linetype = 'dashed')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
q+geom_text()
