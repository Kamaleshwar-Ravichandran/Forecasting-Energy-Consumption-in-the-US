#=============================================
#BANA7050 Forecasting and Timeseries Methods
#Forecasting Energy Consumption in the US
#Date last modified: 29-April-2020
#=============================================

#Install the below packages if running for the first time
#To run: select entire code (ctrl+A) and press (ctrl+Enter)

#install.packages("shiny")
#install.packages("dplyr")
#install.packages("forecast")
#install.packages("shinydashboard")
#install.packages("shinyWidgets")
#install.packages("curl")

library(shiny)
library(dplyr)
library(forecast)
library(shinydashboard)
library(shinyWidgets)
library(curl)

ui<-fluidPage(
  titlePanel("Forecasting Energy Consumption in the US"),
  fluidRow(
    box(width=2,sliderInput("AR_Input1", "AR Order",
                min = 0, max = 10, value = 0, step = 1
    )),
    box(width=2,sliderInput("Diff_Input1", "Order of Differencing",
                min = 0, max = 10, value = 0, step = 1
    )),
    box(width=2,sliderInput("MA_Input1", "MA Order",
                min = 0, max = 10, value = 0, step = 1
    )),
    box(width=2,sliderInput("AR_Input2", "AR Order (Seasonal)",
                min = 0, max = 10, value = 0, step = 1
    )),
    box(width=2,sliderInput("Diff_Input2", "Order of Differencing (Seasonal)",
                min = 0, max = 10, value = 0, step = 1
    )),
    box(width=2,sliderInput("MA_Input2", "MA Order (Seasonal)",
                min = 0, max = 10, value = 0, step = 1
    ))
  ),
  useShinydashboard(),
  fluidRow(
    box(width = 3,
        radioButtons("order", "ARIMA Order", choices = c("Auto - Model orders will be set based our analysis"= "1", "Manual - Set the Model orders from above Slider bars" = "2"), selected = "1")),
    valueBox(
      value=htmlOutput("AR_Value1a"), tags$p("AR order", style = "font-size: 150%;"), icon = icon("line-chart"), color = "maroon", width = 3
    ),
    valueBox(
      value=htmlOutput("Diff_Value1a"), tags$p("Order of Differencing", style = "font-size: 150%;"), icon = icon("line-chart"), color = "green", width = 3
    ),
    valueBox(
      value=htmlOutput("MA_Value1a"), tags$p("MA order", style = "font-size: 150%;"), icon = icon("line-chart"), color = "yellow", width = 3
    )
  ),
  fluidRow(
    box(width = 3,
        selectInput("category","Select a Category",c("Total Fossil Fuels Consumption","Total Primary Energy Consumption","Total Renewable Energy Consumption"))
        ),
    valueBox(
      value=htmlOutput("AR_Value1b"), tags$p("AR order (Seasonal)", style = "font-size: 150%;"), icon = icon("line-chart"), color = "red", width = 3
    ),
    valueBox(
      value=htmlOutput("Diff_Value1b"), tags$p("Order of Differencing (Seasonal)", style = "font-size: 150%;"), icon = icon("line-chart"), color = "blue", width = 3
    ),
    valueBox(
      value=htmlOutput("MA_Value1b"), tags$p("MA order (Seasonal)", style = "font-size: 150%;"), icon = icon("line-chart"), color = "purple", width = 3
    )
  ),
  navlistPanel(widths = c(3,9),
    tabPanel("Forecast Plot",
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Forecast Plot",
                  plotOutput("ForecastPlot")
                   )
              ) 
      ),
      tabPanel("ACF & PACF Plots",
              fluidRow(
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Time Series Plot",
                  plotOutput("TSPlot")
                ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "ACF Plot",
                  plotOutput("ACFPlot")
                ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "PACF Plot",
                  plotOutput("PACFPlot")
                )
              )
      )
  )
)


server<-function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^3)
  df <- reactiveValues()
  
  output$ForecastPlot <- renderPlot({
    df$ec_data<-read.csv(curl("https://raw.githubusercontent.com/Kamaleshwar-Ravichandran/Forecasting-Energy-Consumption-in-the-US/master/MEC.csv"))
    ec_filter <- df$ec_data %>% select(YYYY,MM,Value, Description)
    ec_filtered <- ec_filter %>% filter(MM!=13)
    
    #category dropdown options
    if(input$category=="Total Fossil Fuels Consumption"){
    ec_tot_primary <- ec_filtered %>% filter(Description == "Total Fossil Fuels Consumption")
    
    #updating the inputs with order values got from analysis
    if(input$order==1){
      updateSliderInput(session, "AR_Input1", value = 1)
      updateSliderInput(session, "MA_Input1", value = 2)
      updateSliderInput(session, "Diff_Input1", value = 0)
      updateSliderInput(session, "AR_Input2", value = 2)
      updateSliderInput(session, "MA_Input2", value = 1)
      updateSliderInput(session, "Diff_Input2", value = 1)
    }
    }
    if(input$category=="Total Primary Energy Consumption"){
      ec_tot_primary <- ec_filtered %>% filter(Description == "Total Primary Energy Consumption")
      
    if(input$order==1){
      updateSliderInput(session, "AR_Input1", value = 1)
      updateSliderInput(session, "MA_Input1", value = 2)
      updateSliderInput(session, "Diff_Input1", value = 0)
      updateSliderInput(session, "AR_Input2", value = 2)
      updateSliderInput(session, "MA_Input2", value = 1)
      updateSliderInput(session, "Diff_Input2", value = 1)
    }      
      
    }
    if(input$category=="Total Renewable Energy Consumption"){
      ec_tot_primary <- ec_filtered %>% filter(Description == "Total Renewable Energy Consumption")
    
    if(input$order==1){
      updateSliderInput(session, "AR_Input1", value = 2)
      updateSliderInput(session, "MA_Input1", value = 2)
      updateSliderInput(session, "Diff_Input1", value = 1)
      updateSliderInput(session, "AR_Input2", value = 1)
      updateSliderInput(session, "MA_Input2", value = 2)
      updateSliderInput(session, "Diff_Input2", value = 1)
    }  
    }
    ec_final <- ec_tot_primary[,-4]
    names(ec_final)[1] <- "Year"
    names(ec_final)[2] <- "Month"
    names(ec_final)[3] <- "Consumption"
    df$ec_ts <- ts(ec_final["Consumption"], start = c(1973,1),frequency = 12)
    
    ec_fit <- arima(df$ec_ts,order=c(input$AR_Input1,input$Diff_Input1,input$MA_Input1),seasonal= list(order=c(input$AR_Input2,input$Diff_Input2,input$MA_Input2),period=12))
    
    plot(forecast(ec_fit,h=10))
  })
  
  #plots
  output$TSPlot <- renderPlot({
    plot.ts(df$ec_ts)
  })
  
  output$ACFPlot <- renderPlot({
    acf(df$ec_ts)
  })
  
  output$PACFPlot <- renderPlot({
    pacf(df$ec_ts)
  })
  
  #value box values with model orders
  output$AR_Value1a <- renderText({
    prettyNum(input$AR_Input1)
  })
  
  output$MA_Value1a <- renderText({
    prettyNum(input$MA_Input1)
  })
  
  output$Diff_Value1a <- renderText({
    prettyNum(input$Diff_Input1)
  })
  
  output$AR_Value1b <- renderText({
    prettyNum(input$AR_Input2)
  })

  output$MA_Value1b <- renderText({
    prettyNum(input$MA_Input2)
  })
  
  output$Diff_Value1b <- renderText({
    prettyNum(input$Diff_Input2)
  })
  
}

shinyApp(ui = ui, server = server)
