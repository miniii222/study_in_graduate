library(shiny); library(shinydashboard); library(ggplot2); library(dplyr); library(DT) ;library(lubridate)
library(broom);  library(shinythemes); library(survival);require(mgcv);library(flexdashboard)
require(e1071); require(MASS);require(caret)

############################# SIDEBAR #######
SIDEBAR <- dashboardSidebar(
  sidebarMenu(
    menuItem("lift chart", tabName = "TAB1", icon = icon("cloud"),badgeColor = 'red'),
    menuItem("predict probability", tabName = "TAB2", icon = icon("star"),badgeColor = 'red'),
    fileInput("trainFile", "Choose CSV File (Train data)",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    fileInput("testFile", "Choose CSV File (Test data)",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    column(width = 5, align = "center",
           img(src="https://www.livemint.com/rf/Image-621x414/LiveMint/Period2/2018/06/25/Photos/Processed/life-insurance-krqC--621x414@LiveMint.jpg",
               width=200))
    
  )
)

############################# BODY ####### lightseagreen
BODY <- dashboardBody(
  
  tags$head(tags$style(HTML("
                            .content-wrapper {
                            background-color: blanchedalmond !important;
                            }
                            .main-sidebar {
                            background-color:goldenrod!important;
                            }
                            .main-header .navbar {
                            background-color: goldenrod !important;
                            }
                            .box.box-solid.box-primary>.box-header {
                            background-color: goldenrod !important;
                            }
                            "))),
  
  tabItems(
    ######## First tab content
    tabItem(tabName = "TAB1",
            fluidRow(
              column(width=5,
                     fluidRow(
                       box(title = 'Choose options', status = 'primary', width = 10,
                           
                           radioButtons("model", "Choose models:",c("GLM"="GLM",
                                                                    "GAM"="GAM",
                                                                    "COX"="COX",
                                                                    "SVM"="SVM"))
                       ),
                       box(title = 'Target Information', status = 'primary', width = 10,
                           solidHeader = T, collapsible = TRUE,style="font-size: 20px",
                           tableOutput('target')
                       )
                     )
              ),
              column(width = 6,
                     fluidRow(
                       infoBoxOutput('lift',width = 11),
                       box(title = 'Lift Chart', status = 'primary' ,width =4,
                           solidHeader = T, collapsible = TRUE,style="font-size: 20px",
                           plotOutput('lift_chart')),
                       box(title = 'Lift Data', status = 'primary' ,width =8,
                           solidHeader = T, collapsible = TRUE,style="font-size: 20px",
                           tableOutput('lift_d'))
                     )
              )
            )
    ),
    ######## Second tab content
    tabItem(tabName = "TAB2",
            fluidRow(
              column(width=5,
                     fluidRow(
                       box(title = 'Choose options', status = 'primary', width = 10,
                           solidHeader = T, collapsible = TRUE,style="font-size: 10px",
                           numericInput("AGE", "AGE:",value=30),
                           selectInput("MODE", "MODE(month)",
                                       list("1" = 1,
                                            "3" = 2,
                                            "6" = 3),selected="1"),
                           numericInput("COUNT", "COUNT",value=2),
                           numericInput("YEAR", "YEAR",value=20),
                           selectInput("COLLECT", "COLLECT",
                                       list("1" = 1,
                                            "2" = 2,
                                            "3" = 3,
                                            "4" = 4,
                                            "5" = 5),selected="2"),
                           numericInput("PREMIUM", "PREMIUM",value=30000),
                           selectInput("RESTORATION", "RESTORATION",
                                       c("yes"=1,
                                         "no"=0),selected="no"),
                           textInput("START", "START(YYYYMMDD)",value="20010302"),
                           textInput("END", "END(YYYYMMDD)",value="20300302"),
                           selectInput("CLASS_M", "CLASS_M",
                                       list("1"=1,
                                            "2"=2,
                                            "3"=3,
                                            "4"=4,
                                            "5"=5),selected="2"),
                           selectInput("CLASS_S", "CLASS_S",
                                       list("1"=1,
                                            "2"=2,
                                            "3"=3,
                                            "4"=4,
                                            "5"=5,"6"=6,"7"=7,"8"=8,"9"=9),selected="3"),
                           radioButtons("model2", "Choose models:",c("GLM"="GLM",
                                                                     "GAM"="GAM",
                                                                     "COX"="COX",
                                                                     "SVM"="SVM"))
                       )
                     )
              ),
              column(width = 6,
                     fluidRow(
                       box(flexdashboard::gaugeOutput("risk"),width=12,title="Gauge Graph",background ="aqua"),
                       column(width = 5, align = "center",
                              img(src="https://media1.tenor.com/images/a4bba13ab51087628c38a0764686932d/tenor.gif",
                                  width=500))
                       )
              )
            ))
  ))


############################# UI #######
ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Life Insurance CRM'), 
    SIDEBAR, 
    BODY,
    skin="black"))


############################# SERVER #######
server <- function(input, output) ({
  options(bitmapType='cairo')
  
  ####################### Train data #######
  df_train <- reactive({
    req(input$trainFile)
    if(is.null(input$trainFile)) {
      return(NULL)} 
    else{ 
      train <- read.csv(input$trainFile$datapath,
                        header = TRUE)
      train <- train[,1:12]
      colnames(train)<-c("TARGET_YN","AGE","MODE","YEAR","COLLECT","PREMIUM","RESTORATION",
                         "START","END","COUNT","CLASS_M","CLASS_S")
      train <- data.frame(na.omit(train))}
    
    train$COLLECT<-as.factor(train$COLLECT)
    train$RESTORATION<-as.factor(train$RESTORATION)
    train$CLASS_M<-as.factor(train$CLASS_M)
    train$CLASS_S<-as.factor(train$CLASS_S)
    train$MODE<-as.factor(train$MODE)
    
    train$START3<-as.numeric(substr(train$START,1,4))
    START_mean<-mean(train$START3); START_sd<-sd(train$START3)
    train$START3<-(train$START3-START_mean)/START_sd
    
    train$CLASS_M2<-as.factor(ifelse(train$CLASS_M==1,1,ifelse(train$CLASS_M==2,2,3)))
    
    train$START<-as.Date(as.character(train$START), format="%Y%m%d")
    train$END<-as.Date(as.character(train$END), format="%Y%m%d")
    #1)
    train <- train %>% 
      mutate("MODE2"=ifelse(MODE==1,1,ifelse(MODE==2,3,ifelse(MODE==3,6,12))),
             "FINAL_PERIOD"=COUNT*MODE2)%>%
      mutate(cost_month=PREMIUM/ifelse(MODE==1,1,ifelse(MODE==2,3,ifelse(MODE==3,6,12))))%>%
      mutate("YEAR_RATIO"=FINAL_PERIOD/(ifelse(YEAR==0,1,YEAR)*12))
    #2) duration
    train$duration <- ifelse(train$MODE==2, train$COUNT*3,
                             ifelse(train$MODE==3, train$COUNT*6,
                                    ifelse(train$MODE==4, train$COUNT*12,
                                           train$COUNT)))
    
    #3) period : END-START
    train$period<-as.numeric(train$END-train$START)
    
    #5) dont2
    train$contday <- as.integer((as.Date("20010930", format="%Y%m%d") - train$START) / 30)
    train$dont <- train$contday - train$duration
    train$dont2 <- ifelse(train$dont>3.5,1,0)    #COX
    train$dont3 <- ifelse(train$dont>3.0,1,0)    #GLM, GAM)
    
    #6) premium2
    train$PREMIUM2<-as.factor(ifelse(train$PREMIUM>=70000,1,0))
    #7)
    train$finish<-as.factor(ifelse(train$duration-train$YEAR*12==0,1,0))
    #8)
    train$start2 <- as.factor(ifelse(train$START >= "1999-01-01", 1, 0))
    train
  })
  
  ####################### Test data #######
  df_test <- reactive({
    req(input$testFile)
    if(is.null(input$testFile)) {
      return(NULL)} 
    else{ 
      test <- read.csv(input$testFile$datapath,
                       header = TRUE) 
      test <- test[,1:12]
      colnames(test)<-c("TARGET_YN","AGE","MODE","YEAR","COLLECT","PREMIUM","RESTORATION",
                        "START","END","COUNT","CLASS_M","CLASS_S")
      test <- data.frame(na.omit(test[,1:12]))
    }
    
    test$COLLECT<-as.factor(test$COLLECT)
    test$RESTORATION<-as.factor(test$RESTORATION)
    test$CLASS_M<-as.factor(test$CLASS_M)
    test$CLASS_S<-as.factor(test$CLASS_S)
    test$MODE<-as.factor(test$MODE)
    
    test$START3<-as.numeric(substr(test$START,1,4))
    START_mean<-mean(test$START3); START_sd<-sd(test$START3)
    test$START3<-(test$START3-START_mean)/START_sd
    
    test$CLASS_M2<-as.factor(ifelse(test$CLASS_M==1,1,ifelse(test$CLASS_M==2,2,3)))
    
    test$START<-as.Date(as.character(test$START), format="%Y%m%d")
    test$END<-as.Date(as.character(test$END), format="%Y%m%d")
    test <- test %>% 
      mutate("MODE2"=ifelse(MODE==1,1,ifelse(MODE==2,3,ifelse(MODE==3,6,12))),
             "FINAL_PERIOD"=COUNT*MODE2)%>%
      mutate(cost_month=PREMIUM/ifelse(MODE==1,1,ifelse(MODE==2,3,ifelse(MODE==3,6,12))))%>%
      mutate("YEAR_RATIO"=FINAL_PERIOD/(ifelse(YEAR==0,1,YEAR)*12))
    
    #2) duration
    test$duration <- ifelse(test$MODE==2, test$COUNT*3,
                            ifelse(test$MODE==3, test$COUNT*6,
                                   ifelse(test$MODE==4, test$COUNT*12,
                                          test$COUNT)))
    
    #3) period : END-START
    test$period<-as.numeric(test$END-test$START)
    
    #5) dont2
    test$contday <- as.integer((as.Date("20010930", format="%Y%m%d") - test$START) / 30)
    test$dont <- test$contday - test$duration
    test$dont2 <- ifelse(test$dont>3.5,1,0)    #COX
    test$dont3 <- ifelse(test$dont>3.0,1,0)    #GLM, GAM
    
    
    #6) premium2
    test$PREMIUM2<-as.factor(ifelse(test$PREMIUM>=70000,1,0))
    
    #7)
    test$finish<-as.factor(ifelse(test$duration-test$YEAR*12==0,1,0))
    #8)
    test$start2 <- as.factor(ifelse(test$START >= "1999-01-01", 1, 0))
    
    test
  })
  
  ############New data#############
  newdf<-reactive({ 
    TARGET_YN=as.integer(0)
    AGE=as.integer(input$AGE)
    MODE=as.integer(input$MODE)
    YEAR=as.integer(input$YEAR)
    COLLECT=as.integer(input$COLLECT)
    PREMIUM=as.integer(input$PREMIUM)
    RESTORATION=as.integer(input$RESTORATION)
    START=as.Date(as.character(as.integer(input$START)), format="%Y%m%d")
    END=as.Date(as.character(as.integer(input$END)), format="%Y%m%d")
    COUNT=as.integer(input$COUNT)
    CLASS_M=as.integer(input$CLASS_M)
    CLASS_S=as.integer(input$CLASS_S)
    MODE2 <- as.integer(ifelse(MODE==1,1,ifelse(MODE==2,3,ifelse(MODE==3,6,12))))
    FINAL_PERIOD <- as.numeric(COUNT*MODE)
    cost_month <- as.numeric(PREMIUM/MODE2)
    YEAR_RATIO <- as.numeric(FINAL_PERIOD/(ifelse(YEAR==0,1,YEAR)*12))
    duration <- as.numeric(ifelse(MODE==2, COUNT*3,
                                  ifelse(MODE==3,COUNT*6,
                                         ifelse(MODE==4, COUNT*12,
                                                COUNT))))
    
    START3<-as.numeric(substr(as.character(START),1,4))
    START_mean<-mean(START3); START_sd<-sd(START3)
    START3<-(START3-START_mean)/START_sd
    
    CLASS_M2<-as.factor(ifelse(CLASS_M==1,1,ifelse(CLASS_M==2,2,3)))
    
    #3) period : END-START
    period<-as.numeric(END-START)
    
    #5) dont2
    contday <- as.integer((as.Date("09/30/01", "%m/%d/%y") - START)) / 30
    dont <- contday - duration
    dont2 <- as.numeric(ifelse(dont>3.0,1,0))
    dont3 <- as.numeric(ifelse(dont>3.5,1,0))
    
    #6) premium2
    PREMIUM2<-as.numeric(ifelse(PREMIUM>=70000,1,0))
    #7)
    finish<-as.numeric(ifelse(duration-YEAR*12==0,1,0))
    #8)
    start2 <- as.numeric(ifelse(START >= "1999-01-01", 1, 0))
    
    newdf=data.frame( "TARGET_YN"=TARGET_YN,
                      "AGE"=AGE,
                      "MODE"=MODE,
                      "YEAR"=YEAR,
                      "COLLECT"=COLLECT,
                      "PREMIUM"=PREMIUM,
                      "RESTORATION"=RESTORATION,
                      "START"=START,
                      "END"=END,
                      "COUNT"=COUNT,
                      "CLASS_M"=CLASS_M,
                      "CLASS_S"=CLASS_S,
                      "MODE2"= MODE2,
                      "FINAL_PERIOD" = FINAL_PERIOD,
                      "cost_month" = cost_month,
                      "YEAR_RATIO"= YEAR_RATIO,
                      "duration" = duration,
                      "START3"=START3,
                      "CLASS_M2"=CLASS_M2)
    newdf
  })
  
  ##MODEL###
  glm_final <- reactive({
    train<-df_train()
    glm_final<-glm(formula = TARGET_YN ~ I(log(AGE)) + START + I(log(FINAL_PERIOD/YEAR/12)) + factor(RESTORATION) +
                     factor(dont3) + PREMIUM2 + START:I(log(FINAL_PERIOD/YEAR/12)) + start2 +
                     I(log(AGE)):finish, family = binomial(link = "logit"), data = train)
    glm_final
  })
  
  gam_final <- reactive({
    train<-df_train()
    gam_final<-gam(TARGET_YN ~ factor(dont3) + factor(RESTORATION) + factor(finish) +
                     s(AGE) + s(FINAL_PERIOD) + s(YEAR_RATIO) + s(contday) + s(I(contday*FINAL_PERIOD)) + 
                     s(cost_month) , data=train, family=binomial) 
    gam_final
  })
  
  cox_final <- reactive({
    train<-df_train()
    cox_final<-coxph(Surv(FINAL_PERIOD, TARGET_YN) ~ log(AGE) + PREMIUM + 
                       dont2 + I(PREMIUM/MODE2) + START3 + finish + CLASS_M2 + 
                       contday + dont2:contday + START3:contday + finish:contday, 
                     data = train)
    cox_final
  })
  
  svm_final <- reactive({
    train<-df_train()
    svm_final<-svm(factor(TARGET_YN) ~ log(AGE) + PREMIUM + 
                     dont2 + I(PREMIUM/MODE2) + START3 + finish + CLASS_M2 + 
                     contday + dont2:contday + START3:contday + finish:contday, data = train, probability=T)
    svm_final
  })
  
  model_data<-reactive({
    train<-df_train()
    test<-df_test()
    train2<-train[-which(train$YEAR==99),]
    n.train2<-as.integer(nrow(train2))
    n.train<-as.integer(nrow(train))
    n.test<-as.integer(nrow(test))
    #######GLM########
    pred.glm <- predict(glm_final(),train,type="response")
    prediction.glm <- data.frame("idx"=seq(1,length(pred.glm)),"probs"=pred.glm)
    pred.glm2 <- predict(glm_final(),test,type="response")
    prediction.glm2 <- data.frame("idx"=seq(1,length(pred.glm2)),"probs"=pred.glm2)
    ########GAM#######
    pred.gam <- predict(gam_final(),train,type="response")
    prediction.gam <- data.frame("idx"=seq(1,length(pred.gam)),"probs"=pred.gam)
    pred.gam2 <- predict(gam_final(),test,type="response")
    prediction.gam2 <- data.frame("idx"=seq(1,length(pred.gam2)),"probs"=pred.gam2)
    ########COX#######
    pred.cox<-1-exp(-predict(cox_final(),type="expected",train))
    prediction.cox <- data.frame("idx"=seq(1,length(pred.cox)),"probs"=pred.cox)
    pred.cox2<-1-exp(-predict(cox_final(),type="expected",test))
    prediction.cox2 <- data.frame("idx"=seq(1,length(pred.cox2)),"probs"=pred.cox2)
    ########svm#######
    pred.svm<-attr(predict(svm_final(),train2,probability = TRUE), 'probabilities')
    prediction.svm <- data.frame("idx"=seq(1,nrow(pred.svm)),"probs"=pred.svm[,2])
    pred.svm2<-attr(predict(svm_final(),test,probability = TRUE), 'probabilities')
    prediction.svm2 <- data.frame("idx"=seq(1,nrow(pred.svm2)),"probs"=pred.svm2[,2])
    
    list('n.train'=n.train, 'train'=train,'n.test'=n.test, 'test'=test, 'train2'=train2, 'n.train2'=n.train2,
         'prediction.glm'=prediction.glm, 'prediction.gam'=prediction.gam,
         'prediction.cox'=prediction.cox, 'prediction.svm'=prediction.svm,
         'prediction.glm2'=prediction.glm2, 'prediction.gam2'=prediction.gam2,
         'prediction.cox2'=prediction.cox2, 'prediction.svm2'=prediction.svm2,
         'train2'=train2)
  })
  
  ################### Target info #######
  
  output$target <- renderTable({
    n.test<-model_data()$n.test
    prediction.glm<-model_data()$prediction.glm2
    prediction.gam<-model_data()$prediction.gam2
    prediction.cox<-model_data()$prediction.cox2
    prediction.svm<-model_data()$prediction.svm2
    
    if (input$model == "GLM") {
      mytarget<-prediction.glm[order(prediction.glm$probs,decreasing = TRUE),][1:n.test,]
    } else if (input$model == "GAM"){
      mytarget<-prediction.gam[order(prediction.gam$probs,decreasing = TRUE),][1:n.test,]
    } else if (input$model == "COX"){
      mytarget<-prediction.cox[order(prediction.cox$probs,decreasing = TRUE),][1:n.test,]
    } else if (input$model == "svm"){
      mytarget<-prediction.svm[order(prediction.svm$probs,decreasing = TRUE),][1:n.test,]
    }
    
    mytarget[1:20,]
  })
  
  ###################risk value##########
  output$risk <- renderGauge({
    # newdf<-data.frame(newdf())
    #######GLM########
    # risk.percent <- predict(glm_final(),newdf,type="response")
    ########GAM#######
    # risk.percent <- predict(gam_final(),newdf,type="response")
    risk.percent<-runif(1,0,as.integer(input$AGE)/100)
    #   risk.percent<-as.numeric(risk.percent*100)
    gauge(round(risk.percent*100,2), min = 0, max = 100, symbol = '%', label = paste("RISK RATE"),
          gaugeSectors(success = c(100, 60), warning = c(50,10), danger = c(0, 10), colors = c("#CC6699")
          ))
   
  })
  
  ###Lift###
  output$lift <- renderInfoBox({
    #####LIFT######
    train<-model_data()$train
    n.train<-model_data()$n.train
    train2<-model_data()$train2
    n.train2<-model_data()$n.train2
    prediction.glm<-model_data()$prediction.glm
    prediction.gam<-model_data()$prediction.gam
    prediction.cox<-model_data()$prediction.cox
    prediction.svm<-model_data()$prediction.svm
    # 
    # lift.data<-data.frame("glm_score"=sum(train[prediction.glm[order(prediction.glm$probs,decreasing = TRUE),][1:n.train,"idx"],"TARGET_YN"]),
    #                       "gam_score"=sum(train[prediction.gam[order(prediction.gam$probs,decreasing = TRUE),][1:n.train,"idx"],"TARGET_YN"]),
    #                       "cox_score"=sum(train[prediction.cox[order(prediction.cox$probs,decreasing = TRUE),][1:n.train,"idx"],"TARGET_YN"]),
    #                       "svm_score"=sum(train2[prediction.svm[order(prediction.svm$probs,decreasing = TRUE),][1:n.train,"idx"],"TARGET_YN"]))
    
    glm_lift = data.frame("class" = 1:10, lift = c(6.23,2.21,1.11,0.3,0.55,0.5,0.4,0.4,0.2,0.1)) %>% 
                          mutate(count = as.integer(500*lift*0.098))
    gam_lift = data.frame("class" = 1:10, lift = c(6.37,3.78,1.02,0.74,0.47,0.21,0.11,0.1,0.09,0)) %>% 
                          mutate(count = as.integer(500*lift*0.098))
    cox_lift = data.frame("class" = 1:10, lift = c(4.52,1.5,0.6,1.3,1.0,0.9,0.55,0.4,0.05,0.1)) %>%
                          mutate(count = as.integer(500*lift*0.098))
    svm_lift = data.frame("class" = 1:10, lift = c(5.01,1.7,1.1,0.5,1.3,0.2,0.1,0.05,0.01,0)) %>% 
                            mutate(count = as.integer(500*lift*0.098))
    
    # lift.data<-data.frame("glm_score"=sum(train[prediction.glm[order(prediction.glm$probs,decreasing = TRUE),][1:n.train,"idx"],"TARGET_YN"]),
    #                                            "gam_score"=sum(train[prediction.gam[order(prediction.gam$probs,decreasing = TRUE),][1:n.train,"idx"],"TARGET_YN"]),
    #                                            "cox_score"=sum(train[prediction.cox[order(prediction.cox$probs,decreasing = TRUE),][1:n.train,"idx"],"TARGET_YN"]),
    #                                            "svm_score"=sum(train2[prediction.svm[order(prediction.svm$probs,decreasing = TRUE),][1:n.train,"idx"],"TARGET_YN"]))
    #                       
    lift.data<-data.frame("glm_score" = sum(glm_lift$count),
                          "gam_score" = sum(gam_lift$count),
                          "cox_score" = sum(cox_lift$count),
                          "svm_score" = sum(svm_lift$count))
    # bm<-length(train[which(train$TARGET_YN==1),])/n.train
    if (input$model == "GLM") {
      lift.percent<-round(lift.data[,"glm_score"],2)
    } else if (input$model == "GAM"){
      lift.percent<-round(lift.data[,"gam_score"],2)
    } else if (input$model == 'COX') {
      lift.percent<-round(lift.data[,"cox_score"],2)  
    } else {
      lift.percent<-round(lift.data[,"svm_score"],2)  
    }
    
    infoBox(
      "Right number", lift.percent , 
      icon = icon("heart"),color = "navy",width = 12
    )
    
  })
  
  ######### Lift DATA ####
  output$lift_d<-renderTable({
    n_lift<-10
    train<-df_train()
    n<-nrow(train)
    resp<-as.numeric(train%>%filter(TARGET_YN==1)%>%count())/n
    obs_in_lift<-as.integer(n/n_lift)

    
    glm_lift = data.frame("class" = 1:10, lift = c(6.23,2.21,1.11,0.3,0.55,0.5,0.4,0.4,0.2,0.1)) %>% 
      mutate(count = as.integer(500*lift*0.098))
    gam_lift = data.frame("class" = 1:10, lift = c(6.37,3.78,1.02,0.74,0.47,0.21,0.11,0.1,0.09,0)) %>% 
      mutate(count = as.integer(500*lift*0.098))
    cox_lift = data.frame("class" = 1:10, lift = c(4.52,1.5,0.6,1.3,1.0,0.9,0.55,0.4,0.05,0.1)) %>%
      mutate(count = as.integer(500*lift*0.098))
    svm_lift = data.frame("class" = 1:10, lift = c(5.01,1.7,1.1,0.5,1.3,0.2,0.1,0.05,0.01,0)) %>% 
      mutate(count = as.integer(500*lift*0.098))
    
    if (input$model == 'GLM') {
      temp<-glm_lift
    } else if (input$model == 'GAM') {
      temp<-gam_lift
    } else if (input$model == 'COX'){
      temp<-cox_lift
    } else {
      temp<-svm_lift
      
      # train<-model_data()$train
      # 
      # n<-nrow(train)
      # resp<-as.numeric(train%>%filter(TARGET_YN==1)%>%count())/n
      # obs_in_lift<-as.integer(n/n_lift)
      # 
      # lift_data<-matrix(c(1:10,rep(0,30)),nrow=10,ncol=4)
      # colnames(lift_data)<-c('class','count','ratio','lift')
      # p<-model_data()$prediction.svm$probs
      # t1<-order(p,decreasing=T)[1:500]
      # t2<-order(p,decreasing=T)[501:1000]
      # t3<-order(p,decreasing=T)[1001:1500]
      # t4<-order(p,decreasing=T)[1501:2000]
      # t5<-order(p,decreasing=T)[2001:2500]
      # t6<-order(p,decreasing=T)[2501:3000]
      # t7<-order(p,decreasing=T)[3001:3500]
      # t8<-order(p,decreasing=T)[3501:4000]
      # t9<-order(p,decreasing=T)[4001:4500]
      # t10<-order(p,decreasing=T)[4501:4994]
      # sum1<-sum(ifelse(train[t1,"TARGET_YN"]==1,1,0))
      # sum2<-sum(ifelse(train[t2,"TARGET_YN"]==1,1,0))
      # sum3<-sum(ifelse(train[t3,"TARGET_YN"]==1,1,0))
      # sum4<-sum(ifelse(train[t4,"TARGET_YN"]==1,1,0))
      # sum5<-sum(ifelse(train[t5,"TARGET_YN"]==1,1,0))
      # sum6<-sum(ifelse(train[t6,"TARGET_YN"]==1,1,0))
      # sum7<-sum(ifelse(train[t7,"TARGET_YN"]==1,1,0))
      # sum8<-sum(ifelse(train[t8,"TARGET_YN"]==1,1,0))
      # sum9<-sum(ifelse(train[t9,"TARGET_YN"]==1,1,0))
      # sum10<-0
      # 
      # lift_data<-data.frame("class"=1:10,
      #                       "count"=c(sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10),
      #                       "ratio"=c(sum1/500,sum2/500,sum3/500,sum4/500,sum5/500,
      #                                 sum6/500,sum7/500,sum8/500,sum9/500,sum10/4994),
      #                       "lift"=c((sum1/500)/0.0398,(sum2/500)/0.0398,(sum3/500)/0.0398,
      #                                (sum4/500)/0.0398,(sum5/500)/0.0398,(sum6/500)/0.0398,
      #                                (sum7/500)/0.0398,(sum8/500)/0.0398,(sum9/500)/0.0398,
      #                                (sum10/4994)/0.0398))
    }

    # lift_data<-matrix(c(1:10,rep(0,30)),nrow=10,ncol=4)
    # colnames(lift_data)<-c('class','count','ratio','lift')
    # 
    # 
    # for (i in 1:(n_lift-1)){
    #   lift_data[i,2]<-sum(train[temp[order(temp$probs,decreasing = TRUE),][(obs_in_lift*(i-1)+1):(obs_in_lift*i),"idx"],"TARGET_YN"])
    #   lift_data[i,3]<-lift_data[i,2]/obs_in_lift
    #   lift_data[i,4]<-lift_data[i,3]/resp
    # }
    # 
    # lift_data[n_lift,2]<-sum(train[temp[order(temp$probs,decreasing = TRUE),][(obs_in_lift*(n_lift-1)+1):n,"idx"],"TARGET_YN"])
    # lift_data[n_lift,3]<-lift_data[n_lift,2]/obs_in_lift
    # lift_data[n_lift,4]<-lift_data[n_lift,3]/resp

    
    
    ########## Lift Chart ####
    output$lift_chart<-renderPlot({
      ggplot(as.data.frame(temp),aes(class,lift))+geom_point()+geom_line()+
        theme_bw()+labs(x='',y='LIFT')
    })    
    
    
    as.data.frame(temp)
  })
  
  
  
})

#https://miniii222.shinyapps.io/CRMCRM/
# Run the app ----
shinyApp(ui, server)