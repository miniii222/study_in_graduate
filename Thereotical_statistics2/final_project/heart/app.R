library(shiny); library(shinydashboard); library(ggplot2); library(dplyr); library(DT)
library(broom);  library(shinythemes); library(survival);library(flexdashboard)
require(caret);require(readxl);library(survival);library(plotly)

############################# SIDEBAR #######
SIDEBAR <- dashboardSidebar(
  sidebarMenu(
    menuItem("Coronary heart disease",tabName = "TAB1", icon = icon("heart")),
    fileInput("File", "Upload File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".xlsx")),
    
    column(width = 5, align = "center",
           img(src="https://healthblog.uofmhealth.org/sites/consumer/files/2018-02/remember-the-heart-3.gif",
               width=200),
           
    radioButtons("model", "Models",c("COX"="COX","ALT"="ALT"),
                        inline = F)      
           
           )
    
    
    

  )
)

############################# BODY ####### 
BODY <- dashboardBody(
  tags$head(tags$style(HTML("
                            .content-wrapper {
                            background-color: rosybrown1 !important;
                            }.main-sidebar {
                            background-color:palevioletred!important;
                            }.main-header .navbar {
                            background-color: palevioletred !important;
                            }.box.box-solid.box-primary>.box-header {
                            background-color: thistle !important;
                            }.box.box-solid.box-primary {
                            border-color: thistle !important;
                            }.box.box-solid.box-info>.box-header {
                            background-color: thistle !important;
                            }.box.box-solid.box-info {
                            border-color: thistle !important;
                            }.box.box-solid.box-warning>.box-header {
                            background-color: white !important;
                            color: black !important;
                            }
                            # .box.box-solid.box-warning {
                            # border-color: darkslategray !important;
                            # }
                            
                            "))),
  
  tabItems(
    ######## First tab content
    tabItem(tabName = "TAB1",
            fluidRow(
              #**********************************************************************************
              # input
              #**********************************************************************************
              box(title = 'Information Input', status = 'info',width=6,
                  solidHeader = T,style="font-size: 13px",
                  flowLayout(
                    textInput('sbp','sbp : systolic blood pressure (mmHg)',value=106),
                    textInput('dbp','dbp : diastolic blood pressure (mmHg)',value=68),
                    textInput('scl','scl : serum choleterol (mg/100ml)',value=239),
                    textInput('age','age in years',value=60),
                    textInput('weight','weight (kg)',value=60),
                    textInput('height','weight (cm)',value=180),
                    textInput('month','month : month of year in which baseline exam occured',value=1),
                    sliderInput('year','year',value=15,min=0,max=50,step=5),
                    selectInput("sex", "sex",
                                c("male" = "male",
                                  "female" = "female"))
                  )),
              #**********************************************************************************
              # gaugebar of probability
              #**********************************************************************************
              column(width=5,
                     box(gaugeOutput("risk"),width=12,title="Gauge Graph",background="light-blue"))
            ),
            #**********************************************************************************
            # Choose model
            #**********************************************************************************
            fluidRow(
              # box(title = 'Model', status = 'primary',width=3,
              #     solidHeader = T,style="font-size: 16px",
              #     column(width=12,
              #            fluidRow(
              #              box(title = strong('Choose models:'), status = 'warning', width = 12,
              #                  solidHeader=T,style="font-size: 14px",
              #                  radioButtons("model", "",c("COX"="COX","ALT"="ALT"),
              #                               inline = TRUE)
              #              )
              #            )
              #     )
              # ),
              #**********************************************************************************
              # Q-Q Plot
              #**********************************************************************************
              box(title='Plot',status='primary',width=4,
                  solidHeader=T,style="font-size: 16px",
                  plotlyOutput('plot')
              ),
              #**********************************************************************************
              # Probability Chart of uploaded data
              #**********************************************************************************
              box(title='Probability Chart',status='info',width=3,
                  solidHeader=T,style="font-size: 16px",
                  column(width=4,
                         fluidRow(
                           box(div(style='width:300px;height:400px; overflow-y: scroll',tableOutput('prob')))))
              ))
              #**********************************************************************************
              # Probability of input data
              #**********************************************************************************
            # ,infoBoxOutput('risk2',width=2)
            )
  ))


############################# UI ########################################
ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Survival Analysis'), 
    SIDEBAR, 
    BODY,
    skin="black"
  ))


############################# SERVER #####################################
server <- function(input, output,session) {
  
  #**********************************************************************************
  # import data
  #**********************************************************************************
  df <- reactive({
    req(input$File)
    if(is.null(input$File)) {
      return(NULL)} 
    else{
      chd<-read.csv(input$File$datapath)
      chd = chd %>% mutate(tstar=age*365.25+followup)
      chd$scli<-ifelse(chd$scl>260,1,0)
      chd$sbpi<-as.factor(ifelse(chd$sbp<=120,1,ifelse(chd$sbp<=139,2,ifelse(chd$sbp<150,3,4))))
      chd$dbpi<-as.factor(ifelse(chd$dbp<=80,1,ifelse(chd$dbp<=89,2,ifelse(chd$dbp<100,3,4))))
      chd$age2<-ifelse(chd$age>55,1,0)
      chd$scl2<-ifelse(chd$scl>420,1,0)
      chd$sbp2<-ifelse(chd$sbp<91,1,0);
      chd$dbp2<-ifelse(chd$dbp<60,1,0) #60일 때 22739.18
      chd$sbp3<-ifelse(chd$sbp>260,1,0)
    }
    chd
  })
  #**********************************************************************************
  # input data
  #**********************************************************************************
  df_input <- reactive({
    if (input$sex=="male") {sex=as.numeric(1)} else if (input$sex=="female") {sex=as.numeric(0)}
    chd<-tibble('sbp'=as.numeric(input$sbp),'dbp'=as.numeric(input$dbp),'age'=as.numeric(input$age),
                'scl'=as.numeric(input$scl),'bmi'=as.numeric(input$weight) / (as.numeric(input$height)/100)^2,
                'month'=as.numeric(input$month))
    
    chd = chd %>% mutate(tstar=age*365.25+3650,
                         sex=sex)
    
    chd$scli<-ifelse(chd$scl>260,1,0)
    chd$sbpi<-as.factor(ifelse(chd$sbp<=120,1,ifelse(chd$sbp<=139,2,ifelse(chd$sbp<150,3,4))))
    chd$dbpi<-as.factor(ifelse(chd$dbp<=80,1,ifelse(chd$dbp<=89,2,ifelse(chd$dbp<100,3,4))))
    chd$age2<-ifelse(chd$age>55,1,0)
    chd$scl2<-ifelse(chd$scl>420,1,0)
    chd$sbp2<-ifelse(chd$sbp<91,1,0);
    chd$dbp2<-ifelse(chd$dbp<60,1,0) #60일 때 22739.18
    chd$sbp3<-ifelse(chd$sbp>260,1,0)
    
    chd
  })
  
  
  #**********************************************************************************
  # FInal Model
  #**********************************************************************************
  cox_final <- reactive({
    chd<-df()
    cox_final<-coxph(Surv(followup, chdfate) ~ sbp + I(log(dbp)) + scl + age + scli + 
                       dbp2 + scl2 + I(log(bmi)) + sex + age2 + sbp3 + sbp:scl + 
                       sbp:age2 + scl:I(log(dbp)) + I(log(dbp)):age2 + scl:scli + 
                       scl:dbp2 + age:sex + scl2:sex + I(log(bmi)):age2, data = chd)
    cox_final
  })
  alt_final <- reactive({
    chd<-df()
    alt_final<- survreg(formula = Surv(followup, chdfate) ~ age + sex + bmi + 
                          month + scli + dbpi + age:sbp + age:dbp + dbp:scl + age:bmi + 
                          age:sex + scl:I(sbp - dbp), data = chd, dist = "weibull")
    alt_final
  })
  #**********************************************************************************
  # gauge bar
  #**********************************************************************************
  output$risk<-renderGauge({
    if (input$model=='COX'){
      chd<-df_input()
      H<-basehaz(cox_final(),centered=TRUE)
      lmH <- lm(log(H$hazard)~log(H$time))
      H_t <- exp(lmH$coefficients[1]+lmH$coefficients[2]*log(365*as.numeric(input$year)))
      prob<-as.numeric(1-exp(-H_t)^predict(cox_final(),chd,type="risk"))
    } else if (input$model=='ALT'){
      chd<-df_input()
      prob<-as.numeric(1-exp(-exp((log(365*as.numeric(input$year))-predict(alt_final(),chd,type="link"))/alt_final()$scale)))
    }
    prob<-round(prob,6)
    gauge(prob*100,min=0,max=100,symbol='%',label=paste("Risk"),
          gaugeSectors(success=c(0,14.9999),warning=c(15,34.9999),danger=c(35,100),
                       colors=c("#ABDDA4","#FDAE61","#D53E4F")))
  })
  #**********************************************************************************
  # infobox (probability of input data)
  #**********************************************************************************
  # output$risk2 <- renderInfoBox({
  #   if (input$model=='COX'){
  #     chd<-df_input()
  #     H<-basehaz(cox_final(),centered=TRUE)
  #     lmH <- lm(log(H$hazard)~log(H$time))
  #     H_t <- exp(lmH$coefficients[1]+lmH$coefficients[2]*log(365*as.numeric(input$year)))
  #     prob<-as.numeric(1-exp(-H_t)^predict(cox_final(),chd,type="risk"))
  #   } else if (input$model=='ALT'){
  #     chd<-df_input()
  #     prob<-1-exp(-exp((log(365*as.numeric(input$year))-predict(alt_final(),chd,type="link"))/alt_final()$scale))
  #   } 
  #   prob<-round(prob,4)
  #   p<-paste0(prob*100,"%")
  #   infoBox(
  #     "Prob of CHD", tags$p(style = "font-size: 200%;font-weight: bold;", p), icon = icon("info-circle", lib = "font-awesome"),
  #     color = "green", fill =TRUE, width = 3
  #   )
  # }) 
  #**********************************************************************************
  # table (probability of uploaded data)
  #**********************************************************************************
  output$prob<-renderTable({
    if (input$model=='COX'){
      chd<-df()
      H<-basehaz(cox_final(),centered=TRUE)
      lmH <- lm(log(H$hazard)~log(H$time))
      H_t <- exp(lmH$coefficients[1]+lmH$coefficients[2]*log(3650))
      prob<-data.frame(as.numeric(1-exp(-H_t)^predict(cox_final(),chd,type="risk")))
    } else if (input$model=='ALT'){
      chd<-df()
      prob<-data.frame(1-exp(-exp((log(3650)-predict(alt_final(),chd,type="link"))/alt_final()$scale)))
    } 
    idx<-order(prob,decreasing = TRUE)
    idx<-as.integer(idx)
    cbind(idx,prob[idx,])
  })
  #**********************************************************************************
  # plot (probability of heart disease)
  #**********************************************************************************
  output$plot<-renderPlotly({
    if(input$model=='COX'){
      chd<-df_input()
      t<-seq(0,60,0.1)
      H<-basehaz(cox_final(),centered=TRUE)
      lmH <- lm(log(H$hazard)~log(H$time))
      H_t <- exp(lmH$coefficients[1]+lmH$coefficients[2]*log(365*t))
      prob<-1-exp(-H_t)^predict(cox_final(),chd,type="risk")
      d<-data.frame(p1=prob,t=t)
      ggplot(d,aes(t,p1))+geom_line()+geom_vline(aes(xintercept = 10),color='blue')+
        geom_vline(aes(xintercept=as.numeric(input$year)),color='red')+theme_bw()
    } else if (input$model=='ALT'){
      chd<-df_input()
      t<-seq(0,60,0.1)
      p1<-1-exp(-exp((log(t*365)-predict(alt_final(),chd,type="link"))/alt_final()$scale))
      d<-data.frame(p1=p1,t=t)
      ggplot(d,aes(t,p1))+geom_line()+geom_vline(aes(xintercept = 10),color='blue')+
        geom_vline(aes(xintercept=as.numeric(input$year)),color='red')+theme_bw()
    }
    
  })
}

#https://miniii222.shinyapps.io/heart/
# Run the app ----
shinyApp(ui, server)