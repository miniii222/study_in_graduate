# Define UI for data upload app ----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(broom)
library(dplyr)
library(shinythemes)

result_bass<-function (a,b,c) {
  m1 = (-b + sqrt(b^2-4*a*c))/(2*c)
  m2 = (-b - sqrt(b^2-4*a*c))/(2*c)
  if (m1>0){
    mhat<-m1
  } else {mhat<-m2}
  return(mhat)
}

sidebar <- dashboardSidebar(
  fileInput("selFile", "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  # Input: Checkbox if file has header ----
  checkboxInput("header", "Header", TRUE),
  
  #movie preview
  actionButton("movie1", "With GOD"),
  actionButton("movie2", "Avengers:Infinity war"),
  actionButton("movie3", "Your Marriage"),
  actionButton("movie4", "Bohemian Raphsody"),
  
  # Input: Select separator ----
  radioButtons("sep", "Separator",
               choices = c(Comma = ",",
                           Semicolon = ";",
                           Tab = "\t"),
               selected = ","),
  
  radioButtons("model", "Choose the Diffusion Model:",
               c("Bass" = "Bass",
                 "Gumbel" = "Gumbel",
                 "Logistic" = "Logistic",
                 "Exponential" = "Exponential")),
  radioButtons("Method", "Choose the Estimation Method:",
               c("OLS" = "OLS",
                 "Q-Q Plot" = "Q-Q Plot")),
  checkboxInput("weekend", "Weekend effect", 
                FALSE),
  
  sliderInput("t", "Number of recent data", min=1, max=60, value=c(0,30), 
              animate=animationOptions(100)),
  sliderInput("n", "Number of future data", min=1, max=20, value=1, 
              animate=animationOptions(100))
  
)

## Body content

body <- dashboardBody(
  fluidRow(
    
    #first row
    column(width=12,
           fluidRow(
             box( title = "PREVIEW", status = "warning",
                  width = 12,
                  solidHeader = T, collapsible = TRUE,
                  column(width = 6,
                         div(htmlOutput("video"), style = "font-size:120%"),
                         style = "height:300px; overflow-y: scroll"
                  ),
                  
              column(width=5,
                         fluidRow(
                           tabBox(width=12,
                                  height=NULL,
                                  title="Prediction",
                                  id = "tabset2",
                                  tabPanel("data", "", tableOutput("prediction"), 
                                           style = "height:300px; overflow-y: scroll"))
                         )) 
             )
           )),
    
    #second row
    column(width=12,
           fluidRow(
             tabBox(width = 5,
                    height=5,
                    id = "tabset1",
                    tabPanel("Estimation", "", tableOutput('estimate'), style = "font-size:120%", height=4),
                    tabPanel("QQplot", "", plotOutput('qqplot'))
                    
             ),
           box(title = "audiences", status = "warning",
                width = 4,
                solidHeader = T, collapsible = TRUE,
                column(width =12,
                       h1(textOutput("text"), style = "font-size:150%")
                       
                )
           ))
  )
)
)

ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Movie Demand Prediction'), sidebar, body)
)

# Define server logic to read selected file ----
server <- function(input, output) {
  options(bitmapType='cairo')
  
  dfInput <- reactive({
    req(input$selFile)
    if(is.null(input$selFile)) {
      return(NULL)} 
    else{ 
      df <- read.csv(input$selFile$datapath,
                     header = TRUE)
      df<-data.frame(na.omit(df))}
  })
  
  dfdata <- reactive({
    df<-dfInput()
    
    #here!
    if(ncol(df)==4){
      
      if(input$weekend==TRUE){
        
        aa<-df %>% filter(weekend==1)
        df<-rbind(df,aa) %>% arrange(T)
        
        df[df$weekend==1,'S']<-df[df$weekend==1,'S']/2
        
        if(sum(df$weekend==2)>1){
          
          bb<-df %>% filter(weekend==2)
          df<-rbind(df,bb,bb) %>% arrange(T)
          
          df[df$weekend==2,'S']<-df[df$weekend==2,'S']/3
          
        }
        
        y1<-df$Y[1]
        df$Y<-cumsum(df$S)
        df$Y[1]<-y1
      }
      
      df<-cbind(1:nrow(df),df)
      df <- df %>% mutate(Y_lag = dplyr::lag(df[,4],1))
      df[is.na(df)] <- 0
      colnames(df)<-c("I","T","S","Y","weekend","Y_lag")
      df<-df[c(1,2,3,4,6,5)]
      df<-df[input$t[1]:min(nrow(df),input$t[2]),]
      
      
    }else{
      
      df<-cbind(1:nrow(df),df)
      df <- df %>% mutate(Yt = cumsum(df[,3]))
      df <- df %>% mutate(Y_lag = dplyr::lag(df[,4],1))
      df[is.na(df)] <- 0
      colnames(df)<-c("I","T","S","Y","Y_lag")
      df<-df[input$t[1]:min(nrow(df),input$t[2]),]
    }
    
  })
  
  mymodel <- reactive({
    df1<-dfdata()
    df2<-df1[-1,]
    if (input$model == "Bass"){
      fit<-lm(S ~ Y_lag+I(Y_lag**2),data=df1)
    } else if (input$model == "Logistic"){
      fit<-lm(S ~ Y_lag+I(Y_lag**2) -1 ,data=df2)
    } else if(input$model == "Gumbel"){
      fit<-lm(S ~ Y_lag+I(Y_lag*log(Y_lag)) -1 ,data=df2)
    } else if(input$model =="Exponential"){
      fit<-lm(S ~ Y_lag ,data=df1)
    }
    a<-fit$coefficients[1]
    b<-fit$coefficients[2]
    c<-fit$coefficients[3]
    
    list(a=a,b=b,c=c)
  })
  
  result <- reactive({
    if (input$Method == "OLS"){
      a=mymodel()$a
      b=mymodel()$b
      c=mymodel()$c
      if (input$model == "Bass"){
        result1<- data.frame("m"=result_bass(a,b,c),
                             "p" = a/result_bass(a,b,c),
                             "q" = -1*result_bass(a,b,c)*c)
        return(result1)
      } else if (input$model == "Gumbel"){
        result2<- data.frame("m"=exp(-a/b),
                             "q" = -b)
        return(result2)
      } else if (input$model == "Logistic"){
        result3<- data.frame("m"= -a/b,
                             "q" = a)
        return(result3)
      } else if (input$model == "Exponential"){
        result4<-data.frame("m"=-a/b,
                            "p"=-b)
        return(result4)
      }
    } else if (input$Method == "Q-Q Plot"){
      df1<-dfdata()
      a=mymodel()$a
      b=mymodel()$b
      c=mymodel()$c
      if (input$model == "Bass"){
        result1<- c("m"=NA,
                    "p" = NA,
                    "q" = NA)
        return(result1)
      } else if (input$model == "Gumbel"){
        box2<-data.frame("m"=seq(exp(-a/b)-5000,exp(-a/b)+5000,100),
                         "r-square"=rep(0.00000,length(seq(exp(-a/b)-5000,exp(-a/b)+5000,100))))
        m <- seq(exp(-a/b)-5000,exp(-a/b)+5000,100)
        for (i in 1:length(seq(exp(-a/b)-5000,exp(-a/b)+5000,100))){
          u <- df1$Y/(m[i]+1)
          box2[i,2]<-summary(lm( df1$T ~ I(-log(-log(u))),data=df1))$r.squared
        }
        mqq2<-box2[which.max(box2[,2]),1]
        result2<-c("m"=mqq2,
                   "q"=1/(lm( df1$T ~ I(-log(-log(u))),data=df1)$coefficients[2]))
        return(result2)
      } else if (input$model == "Logistic"){
        box3<-data.frame("m"=seq(-(a/b)-5000,-(a/b)+5000,100),
                         "r-square"=rep(0.00000,length(seq(-(a/b)-5000,-(a/b)+5000,100))))
        m <- seq(-(a/b)-5000,-(a/b)+5000,100)
        for (i in 1:length(seq(-(a/b)-5000,-(a/b)+5000,100))){
          u <- df1$Y/(m[i]+1)
          box3[i,2]<-summary(lm( df1$T ~ I(log(u/(1-u))),data=df1))$r.squared
        }
        mqq3<-box3[which.max(box3[,2]),1]
        result3<-c("m"=mqq3,
                   "q"=1/(lm( df1$T ~ I(log(u/(1-u))),data=df1)$coefficients[2]))
        return(result3)
      } else if (input$model == "Exponential"){
        box4<-data.frame("m"=seq(-(a/b)-5000,-(a/b)+5000,100),
                         "r-square"=rep(0.00000,length(seq(-(a/b)-5000,-(a/b)+5000,100))))
        m <- seq(-(a/b)-5000,-(a/b)+5000,100)
        for (i in 1:length(seq(-(a/b)-5000,-(a/b)+5000,100))){
          u <- df1$Y/(m[i]+1)
          box4[i,2]<-summary(lm( df1$T ~ I(-log((1-u))),data=df1))$r.squared
        }
        mqq4<-box4[which.max(box4[,2]),1]
        result4<-c("m"=mqq4,
                   "p"=1/(lm( df1$T ~ I(-log((1-u))),data=df1)$coefficients[2]))
        return(result4)
      }
    }
  })
  
  
  # data output
  terms1 <- reactiveValues(link = NULL)
  
  observeEvent(input$movie1, {
    terms1$link <- "https://www.youtube.com/embed/5O5PVvHTWRo"
  })
  
  observeEvent(input$movie2, {
    terms1$link <- "https://www.youtube.com/embed/xUDhdCsLkjU"
  })
  
  observeEvent(input$movie3, {
    terms1$link <- "https://www.youtube.com/embed/eUO2OGbQv44"
  })
  
  observeEvent(input$movie4, {
    terms1$link <- " https://www.youtube.com/embed/XTZko22Ze3o"
  })
 
  
  
  output$video <- renderUI({
    tags$iframe(src = terms1$link, width = 450, height = 300)
  })

  ## time-series plot
  output$timeseries <- renderPlot({
    ggplot(dfdata())+geom_line(aes(T,S,group=1))+
      ggtitle('Time Series Plot')+
      theme_bw()+theme(panel.grid.minor = element_blank(),
                       plot.title = element_text(face='bold',size=20))
  })
  
  ## estimate
  output$estimate = renderTable({
    result()
  })
  
  ##'Q-Q Plot' 
  output$qqplot = renderPlot({
    if (input$model == "Bass"){
      dfmodel_bass<-dfdata()%>%
        mutate('p'=Y/(result()[,1]+1),
               'F'=(result()[,2]+result()[,3])^(-1)*log((1+(result()[,3]/result()[,2])*p)/(1-p)))
      R_squ1<-round(summary(lm(T~F,data=dfmodel_bass))$r.square,3)
      
      ggplot(dfmodel_bass,aes(F,T))+geom_point()+
        geom_smooth(method='lm',se=F)+theme_bw()+
        labs(title=paste('Bass Q-Q Plot, R-square=', R_squ1),x='',y='time')+
        theme(plot.title = element_text(face='bold',size=20))
    } else if (input$model == "Gumbel"){
      dfmodel_gum<-dfdata()%>%
        mutate('U'=Y/(result()[,1]+1),
               'Ginv'=-log(-log(U)))
      R_squ2<-round(summary(lm(T~Ginv,data=dfmodel_gum))$r.square,3)
      
      ggplot(dfmodel_gum,aes(Ginv,T))+geom_point()+
        geom_smooth(method='lm',se=F)+theme_bw()+
        labs(title=paste('Gumbel Q-Q Plot, R-square=', R_squ2),x='',y='time')+
        theme(plot.title = element_text(face='bold',size=20))
    } else if (input$model == "Logistic"){
      dfmodel_logi<-dfdata()%>%
        mutate('U'=Y/(result()[,1]+1),
               'Ginv'=log(U/(1-U)))
      R_squ3<-round(summary(lm(T~Ginv,data=dfmodel_logi))$r.square,3)
      
      ggplot(dfmodel_logi,aes(Ginv,T))+geom_point()+
        geom_smooth(method='lm',se=F)+theme_bw()+
        labs(title=paste('Logistic Q-Q Plot, R-square=', R_squ3),x='',y='time')+
        theme(plot.title = element_text(face='bold',size=20))
    } else if (input$model == "Exponential"){
      dfmodel_exp<-dfdata()%>%
        mutate('U'=Y/(result()[,1]+1),
               'Ginv'=-log(1-sort(U)))
      R_squ4<-round(summary(lm(T~Ginv,data=dfmodel_exp))$r.square,3)
      
      ggplot(dfmodel_exp,aes(Ginv,T))+geom_point()+
        geom_smooth(method='lm',se=F)+theme_bw()+
        labs(title=paste('Exponential Q-Q Plot, R-square=', R_squ4),x='',y='time')+
        theme(plot.title = element_text(face='bold',size=20))
    }
    
  })
  
  ## prediction
  df_p <- reactive({
    df1<-dfdata()
    df2<-df1[-1,]
    if (input$model == "Bass"){
      fit<-lm(S ~ Y_lag+I(Y_lag**2),data=df1)
    } else if (input$model == "Logistic"){
      fit<-lm(S ~ Y_lag+I(Y_lag**2) -1 ,data=df2)
    } else if(input$model == "Gumbel"){
      fit<-lm(S ~ Y_lag+I(Y_lag*log(Y_lag)) -1 ,data=df2)
    } else if(input$model =="Exponential"){
      fit<-lm(S ~ Y_lag ,data=df1)
    }
    a<-fit$coefficients[1]
    b<-fit$coefficients[2]
    c<-fit$coefficients[3]
    if (input$model == "Bass"){
      for (i in 1:input$n) {
        df1[input$t[2]-input$t[1]+i+1,3] = a+b*df1[input$t[2]-input$t[1]+i,4]+c*(df1[input$t[2]-input$t[1]+i,4]^2)
        df1[input$t[2]-input$t[1]+i+1,4] = df1[input$t[2]-input$t[1]+i+1,3] + df1[input$t[2]-input$t[1]+i,4]
        df1[input$t[2]-input$t[1]+i+1,1] <- as.integer(df1[input$t[2]-input$t[1]+i,1]+1)
        df1[input$t[2]-input$t[1]+i+1,2] <- as.integer(df1[input$t[2]-input$t[1]+i,2]+1)
      }
      df1[,-5]
    } else if (input$model == "Logistic"){
      for (i in 1:input$n) {
        df1[input$t[2]-input$t[1]+i+1,3] = a*df1[input$t[2]-input$t[1]+i,4]+b*(df1[input$t[2]-input$t[1]+i,4]^2)
        df1[input$t[2]-input$t[1]+i+1,4] = df1[input$t[2]-input$t[1]+i+1,3] + df1[input$t[2]-input$t[1]+i,4]
        df1[input$t[2]-input$t[1]+i+1,1] <- as.integer(df1[input$t[2]-input$t[1]+i,1]+1)
        df1[input$t[2]-input$t[1]+i+1,2] <- as.integer(df1[input$t[2]-input$t[1]+i,2]+1)
      }
      df1[,-5]
    } else if(input$model == "Gumbel"){
      for (i in 1:input$n) {
        df1[input$t[2]-input$t[1]+i+1,3] = a*df1[input$t[2]-input$t[1]+i,4]+b*df1[input$t[2]-input$t[1]+i,4]*log(df1[input$t[2]-input$t[1]+i,4])
        df1[input$t[2]-input$t[1]+i+1,4] = df1[input$t[2]-input$t[1]+i+1,3] + df1[input$t[2]-input$t[1]+i,4]
        df1[input$t[2]-input$t[1]+i+1,1] <- as.integer(df1[input$t[2]-input$t[1]+i,1]+1)
        df1[input$t[2]-input$t[1]+i+1,2] <- as.integer(df1[input$t[2]-input$t[1]+i,2]+1)
      }
      df1[,-5]
    } else if(input$model =="Exponential"){
      for (i in 1:input$n) {
        df1[input$t[2]-input$t[1]+i+1,3] = a+b*df1[input$t[2]-input$t[1]+i,4]
        df1[input$t[2]-input$t[1]+i+1,4] = df1[input$t[2]-input$t[1]+i+1,3] + df1[input$t[2]-input$t[1]+i,4]
        df1[input$t[2]-input$t[1]+i+1,1] <- as.integer(df1[input$t[2]-input$t[1]+i,1]+1)
        df1[input$t[2]-input$t[1]+i+1,2] <- as.integer(df1[input$t[2]-input$t[1]+i,2]+1)
      }
      df1[,-5]
    }
    
  })
  
  output$prediction = renderTable({
    df_p <- df_p()
    return(df_p)
  })
  
  output$prediction_plot = renderPlot({
    df<-dfdata(); df_p<-df_p()
    ggplot()+geom_line(data=df_p, aes(x=T, y=S), col=1)+geom_line(data=df_p[nrow(df_p)-input$n+1:nrow(df_p),], aes(x=T, y=S), col=2)+
      ggtitle("Plot of Prediction")+theme_bw()+theme(plot.title = element_text(face='bold',size=20))
  })
  
  output$text <- renderText({
    final_df<-df_p()
    n_final <- nrow(final_df)
    
    paste0("After ",input$n,"day(s) estimated number of audiences with ",
           input$t[2]," days data is ",
           as.integer(final_df[n_final,4]))
  })
  
  
}

  


# Run the app ----
shinyApp(ui, server)
#https://miniii222.shinyapps.io/movie/