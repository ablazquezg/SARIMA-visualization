#################################
#### SHINY APP ##################
#################################

### LIBRARIES ####
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(forecast)
library(dygraphs)


### DASHBOARD ####
header <- dashboardHeader(title = "Energy forecast")

sidebar <- dashboardSidebar(
  sidebarMenu(    
    menuItem("SARIMA model", tabName = "entr1", icon = icon("bar-chart-o"),
             menuSubItem(text="Scenario forecast", tabName = "entr"),
             menuSubItem(text="Application to a new data set", tabName = "newds")),
    menuItem("Residual diagnostic", tabName = "diag", icon = icon(""))
  )
)

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "entr",
            fluidRow(
              column(width = 6,
                     box(dygraphOutput("forecast_plots",height='250px',width='100%'),
                         width = NULL),
                     box(dygraphOutput("error_plots",height='250px',width='100%'),
                         width = NULL),
                     box(dygraphOutput("errorr_plots",height='250px',width='100%'),
                         width = NULL)
              ),
              column(width = 6,box(selectInput("forecast", "Time horizon:",
                                               c("30 minutes (working days)" = "arima.ga"),
                                   width=NULL)
                     ,
                     box(DT::dataTableOutput("accuracy_table"),
                         width=NULL)
              )
            ))
    ),
    
    # Second
    tabItem(tabName = "newds",
            fluidRow(
              column(width = 6,
                     box(dygraphOutput("new_forecast_plots",height='250px',width='100%'),
                         width = NULL),
                     box(dygraphOutput("new_error_plots",height='250px',width='100%'),
                         width = NULL),
                     box(dygraphOutput("new_errorr_plots",height='250px',width='100%'),
                         width = NULL)
              ),
              column(width = 6,box(selectInput("new_forecast", "Time horizon:",
                                               c("30 minutes (working days)"="newfit")),
                                   width=NULL)
                     ,
                     box(DT::dataTableOutput("new_accuracy_table"),
                         width=NULL))
            )
    ),
    
    # Third tab content
    tabItem(tabName = "diag",
            fluidRow(
              column(width = 6,
                     box(plotOutput("diag1",height='500px',width='100%'),
                         width = NULL)
              ),
              column(width = 6,
                     box(selectInput("forecast1", "Time horizon:",
                                     c("30 minutes (working days)" )),
                         width=NULL),
                     box(verbatimTextOutput('boxt'),width=NULL)
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  output$forecast_plots <- renderDygraph({
    if(input$forecast == "arima.ga"){
      result_graph[[1]] 
    } 
  })
  
  output$error_plots <- renderDygraph({
    if(input$forecast == "arima.ga"){
      result_graph[[2]] %>% dyRangeSelector(height = 20, strokeColor = "")
    } 
  })
  
  output$errorr_plots <- renderDygraph({
    if(input$forecast == "arima.ga"){
      result_graph[[3]] %>% dyRangeSelector(height = 20, strokeColor = "")
    } 
  })
  
  output$accuracy_table <- DT::renderDataTable({
    if (input$forecast == "arima.ga") {
      acc <- round(accuracy(fore.ga, test.ts_wd), 4)
      t(acc)
    } 
  })
  
  # Residual check
  
  output$diag1 <- renderPlot({
    if(input$forecast == "arima.ga"){
      checkresiduals(arima.ga)
    }
  })
  
  output$boxt <- renderPrint({
    if (input$forecast == "arima.ga") {
      Box.test(residuals(arima.ga), lag=min(length(train.ts_wd)/5, 2*24*2), type="Ljung")
    } 
  })
  
  
  # Application new data set
  
  output$new_forecast_plots <- renderDygraph({
    if(input$new_forecast == "newfit"){ 
      new_results_graph[[1]] 
    } 
  })
  
  output$new_error_plots <- renderDygraph({
    if(input$new_forecast == "newfit"){
      new_results_graph[[2]] %>% 
        dyRangeSelector(height = 20, strokeColor = "")
    } 
  })
  
  output$new_errorr_plots <- renderDygraph({
    if(input$new_forecast == "newfit"){
      new_results_graph[[3]] %>% dyRangeSelector(height = 20, strokeColor = "")
    } 
  })
  
  output$new_accuracy_table <- DT::renderDataTable({
    if (input$new_forecast == "newfit") {
      new_acc <- round(accuracy(newfit), 4)
      t(new_acc)
    } 
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
