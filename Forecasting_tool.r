# Time-Series Forecasting Tool // Samuel Drew - UC3KR
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(readxl)
library(writexl)
library(forecast)
library(smooth)
library(plotly)
library(openxlsx)

# Define UI  ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Time Series Forecast"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Used for enabling/disabling widgets which will be required later 
      shinyjs::useShinyjs(),
      
      # Input File ----
      fileInput('fileUpload', 'Upload Data', accept = c('.csv', '.xlsx')),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Horizon of Forecast i.e Ahead or last 10% of data
      
      selectInput('forecastHorizon', 'Forecast Horizon', c('Time Steps Ahead', 'Last 10% of Data'), selected = 'Time Steps Ahead'),
      
      #Input: Frequency Selection:
      selectInput("f", "Frequency of Data:", c("365", "52", "12", "Auto-Detect")),
      #force seasonality selection:
      checkboxInput("fh", "Force Seasonality?", FALSE),
      
      # Input: Slider for the number of time steps to forecast ----
      sliderInput('nTimeSteps', 'Number of Time Steps to Forecast', min = 1, max = 365, value = 1),
      
      
      actionButton('generate', 'Generate Forecast', icon = icon('gears'))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabs for ARIMA, ETS, TBATS, etc ----
      tabsetPanel(type = "tabs",
                  tabPanel("ARIMA",
                           br(),
                           fluidRow(
                             column(3,
                                    tableOutput('tableARIMA')
                             ),
                             column(9,
                                    plotlyOutput("plotARIMA"),
                                    br(),
                                    fluidRow(
                                      column(4,
                                             textOutput('MAPE_ARIMA'),
                                             textOutput('RMSE_ARIMA')
                                      ),
                                      column(4, align = 'right', offset = 4,
                                             textInput('filenameARIMA', '', placeholder = 'File Name'),
                                             downloadButton('downloadARIMA', 'Download Excel Spreadsheet')
                                      )
                                    )
                             )
                           ),
                  ),
                  tabPanel("ETS",
                           br(),
                           fluidRow(
                             column(3,
                                    tableOutput('tableETS')
                             ),
                             column(9,
                                    plotlyOutput("plotETS"),
                                    br(),
                                    fluidRow(
                                      column(4,
                                             textOutput('MAPE_ETS'),
                                             textOutput('RMSE_ETS')
                                      ),
                                      column(4, align = 'right', offset = 4,
                                             textInput('filenameETS', '', placeholder = 'File Name'),
                                             downloadButton('downloadETS', 'Download Excel Spreadsheet')
                                      )
                                    )
                             )
                           ),
                  ),
                  tabPanel("TBATS",
                           br(),
                           fluidRow(
                             column(3,
                                    tableOutput('tableTBATS')
                             ),
                             column(9,
                                    plotlyOutput("plotTBATS"),
                                    br(),
                                    fluidRow(
                                      column(4,
                                             textOutput('MAPE_TBATS'),
                                             textOutput('RMSE_TBATS')
                                      ),
                                      column(4, align = 'right', offset = 4,
                                             textInput('filenameTBATS', '', placeholder = 'File Name'),
                                             downloadButton('downloadTBATS', 'Download Excel Spreadsheet')
                                      )
                                    )
                             )
                           ),
                  ),
                  tabPanel("Simple Moving Average",
                           br(),
                           fluidRow(
                             column(3,
                                    tableOutput('tableSMA')
                             ),
                             column(9,
                                    plotlyOutput("plotSMA"),
                                    br(),
                                    fluidRow(
                                      column(4,
                                             textOutput('MAPE_SMA'),
                                             textOutput('RMSE_SMA')
                                      ),
                                      column(4, align = 'right', offset = 4,
                                             textInput('filenameSMA', '', placeholder = 'File Name'),
                                             downloadButton('downloadSMA', 'Download Excel Spreadsheet')
                                      )
                                    )
                             )
                           ),
                  )
      )
      
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {
  
  # Declare the reactive values which rerun the code if their value is changed
  values = reactiveValues(
    dataUser = NULL,
    timeSteps = NULL,
    
    dataARIMA = NULL,
    forecastARIMA = NULL,
    tableARIMA = NULL,
    MAPE_ARIMA = NULL,
    RMSE_ARIMA = NULL,
    
    dataETS = NULL,
    forecastETS = NULL,
    tableETS = NULL,
    MAPE_ETS = NULL,
    RMSE_ETS = NULL,
    
    dataTBATS = NULL,
    forecastTBATS = NULL,
    tableTBATS = NULL,
    MAPE_TBATS = NULL,
    RMSE_TBATS = NULL,
    
    dataSMA = NULL,
    forecastSMA = NULL,
    tableSMA = NULL,
    MAPE_SMA = NULL,
    RMSE_SMA = NULL
  )
  
  # If forecast is for the last 10% of data then disable the nTimeSteps option becuase it is not required in that case
  observe({
    if (input$forecastHorizon == 'Last 10% of Data') {
      shinyjs::disable('nTimeSteps')
    } else {
      shinyjs::enable('nTimeSteps')
    }
  })
  
  # Run this code when the Generate Forecast button is pressed
  observeEvent(input$generate, {
    
    # Check if user has uploaded a file and get the file type i.e csv or xlsx
    if (is.null(input$fileUpload)) {
      sendSweetAlert(session, 'No File Found', 'Please upload a file', type = 'error')
    } else {
      fileName = strsplit(input$fileUpload$datapath, '.', fixed = T)[[1]]
      fileType = fileName[length(fileName)]
    }
    
    # Get the uploaded file
    file = req(input$fileUpload)
    
    # Read the file depending upon the file type i.e csv or xlsx
    if (fileType == 'csv'){
      values$dataUser = read.csv(file$datapath)
    } else if (fileType == 'xlsx'){
      values$dataUser = as.data.frame(read_xlsx(file$datapath, sheet = 1))
    } else {
      sendSweetAlert(session, 'Incorrect File Type', 'Please upload a csv or xlsx file', type = 'error')
    }
    
    # Make sure the file has been read
    req(values$dataUser)
    
    values$dataUser[, 1] = as.Date(values$dataUser[, 1], format = '%d/%m/%Y')
    xl <<- values$dataUser
    # Send an alert to inform the user that the forecast generation process has been started
    sendSweetAlert(session, 'Generating Forecast', 'Please wait...', type = 'info')
    
    # Run this in case of last 10% data prediction
    if (input$forecastHorizon == 'Last 10% of Data') {
      
      
      #### ARIMA ####
      
      
      # Get the number of rows in the last 10% of data
      timeSteps = floor(nrow(values$dataUser)/10)
      
      # Read the first 90% of data for modelling
      if(input$f == '365' || input$f == '52' || input$f == '12'){
        dataARIMA = ts(values$dataUser[1:(nrow(values$dataUser)-timeSteps), c(2)], frequency = as.numeric(input$f))
      } else{
        dataARIMA = ts(values$dataUser[1:(nrow(values$dataUser)-timeSteps), c(2)])
      }
      # Fit the ARIMA model
      if(input$fh == TRUE){
        modelARIMA = auto.arima(dataARIMA, seasonal = TRUE, lambda = 0)
      } else{
        modelARIMA = auto.arima(dataARIMA, lambda = 0)
      }
      # Forecast the remaining 10% of the data
      forecastARIMAlist = forecast::forecast(modelARIMA, h=timeSteps, level = c(95))
      
      # Get the forecast, high and low bounds returned by the ARIMA model
      forecastARIMA = data.frame(Forecast = as.numeric(forecastARIMAlist$mean), High = forecastARIMAlist$upper, Low = forecastARIMAlist$lower)
      # Fix the column names
      colnames(forecastARIMA) = c('Forecast', 'High', 'Low')
      
      # Add NAs to the forecast to make the number of rows for data and forecast be equal which is required for plotting
      values$forecastARIMA = rbind(data.frame(Forecast = rep(NA, length(dataARIMA)), High = rep(NA, length(dataARIMA)), Low = rep(NA, length(dataARIMA))), forecastARIMA)
      values$dataARIMA = data.frame(Data = values$dataUser[,c(2)])
      
      # Format the table to be displayed
      values$tableARIMA = data.frame(Date = values$dataUser[, c(1)], Data = values$dataUser[, c(2)], Forecast = values$forecastARIMA$Forecast)
      
      # Calculate the error metrics
      values$MAPE_ARIMA = mean(abs((values$dataARIMA$Data-values$forecastARIMA$Forecast)/values$dataARIMA$Data), na.rm = T) * 100
      values$RMSE_ARIMA = sqrt(mean((values$forecastARIMA$Forecast - values$dataARIMA$Data)^2, na.rm = T))
      
      
      
      
      
      #### ETS ####
      
      # Get the number of rows in the last 10% of data
      timeSteps = floor(nrow(values$dataUser)/10)
      
      # Read the first 90% of data for modelling
      if(input$f == '365' || input$f == '52' || input$f == '12'){
        dataETS = ts(values$dataUser[1:(nrow(values$dataUser)-timeSteps), c(2)], frequency = as.numeric(input$f))
      } else{
        dataETS = ts(values$dataUser[1:(nrow(values$dataUser)-timeSteps), c(2)])
      }
      
      # Fit the ETS model
      modelETS = ets(dataETS)
      
      # Forecast the remaining 10% of the data
      forecastETSlist = forecast::forecast(modelETS, h=timeSteps, level = c(95))
      
      # Get the forecast, high and low bounds returned by the ETS model
      forecastETS = data.frame(Forecast = as.numeric(forecastETSlist$mean), High = forecastETSlist$upper, Low = forecastETSlist$lower)
      # Fix the column names
      colnames(forecastETS) = c('Forecast', 'High', 'Low')
      
      # Add NAs to the forecast to make the number of rows for data and forecast be equal which is required for plotting
      values$forecastETS = rbind(data.frame(Forecast = rep(NA, length(dataETS)), High = rep(NA, length(dataETS)), Low = rep(NA, length(dataETS))), forecastETS)
      values$dataETS = data.frame(Data = values$dataUser[,c(2)])
      
      # Format the table to be displayed
      values$tableETS = data.frame(Date = values$dataUser[, c(1)], Data = values$dataUser[, c(2)], Forecast = values$forecastETS$Forecast)
      
      # Calculate the error metrics
      values$MAPE_ETS = mean(abs((values$dataETS$Data-values$forecastETS$Forecast)/values$dataETS$Data), na.rm = T) * 100
      values$RMSE_ETS = sqrt(mean((values$forecastETS$Forecast - values$dataETS$Data)^2, na.rm = T))
      
      
      
      
      
      
      #### TBATS ####
      
      
      # Get the number of rows in the last 10% of data
      timeSteps = floor(nrow(values$dataUser)/10)
      
      # Read the first 90% of data for modelling
      if(input$f == '365' || input$f == '52' || input$f == '12'){
        dataTBATS = ts(values$dataUser[1:(nrow(values$dataUser)-timeSteps), c(2)], frequency = as.numeric(input$f))
      } else{
        dataTBATS = ts(values$dataUser[1:(nrow(values$dataUser)-timeSteps), c(2)])
      }
      
      # Fit the TBATS model
      modelTBATS = tbats(dataTBATS)
      
      # Forecast the remaining 10% of the data
      forecastTBATSlist = forecast::forecast(modelTBATS, h=timeSteps, level = c(95))
      
      # Get the forecast, high and low bounds returned by the TBATS model
      forecastTBATS = data.frame(Forecast = as.numeric(forecastTBATSlist$mean), High = forecastTBATSlist$upper, Low = forecastTBATSlist$lower)
      # Fix the column names
      colnames(forecastTBATS) = c('Forecast', 'High', 'Low')
      
      # Add NAs to the forecast to make the number of rows for data and forecast be equal which is required for plotting
      values$forecastTBATS = rbind(data.frame(Forecast = rep(NA, length(dataTBATS)), High = rep(NA, length(dataTBATS)), Low = rep(NA, length(dataTBATS))), forecastTBATS)
      values$dataTBATS = data.frame(Data = values$dataUser[,c(2)])
      
      # Format the table to be displayed
      values$tableTBATS = data.frame(Date = values$dataUser[, c(1)], Data = values$dataUser[, c(2)], Forecast = values$forecastTBATS$Forecast)
      
      # Set the errors to NA becuase forecast is beyond the data horizon
      values$MAPE_TBATS = mean(abs((values$dataTBATS$Data-values$forecastTBATS$Forecast)/values$dataTBATS$Data), na.rm = T) * 100
      values$RMSE_TBATS = sqrt(mean((values$forecastTBATS$Forecast - values$dataTBATS$Data)^2, na.rm = T))
      
      
      
      
      
      #### SMA ####
      
      
      # Get the number of rows in the last 10% of data
      timeSteps = floor(nrow(values$dataUser)/10)
      
      # Read the first 90% of data for modelling
      if(input$f == '365' || input$f == '52' || input$f == '12'){
        dataSMA = ts(values$dataUser[1:(nrow(values$dataUser)-timeSteps), c(2)], frequency = as.numeric(input$f))
      } else{
        dataSMA = ts(values$dataUser[1:(nrow(values$dataUser)-timeSteps), c(2)])
      }
      
      # Fit the SMA model
      modelSMA = sma(dataSMA, h = timeSteps)
      
      # Forecast the remaining 10% of the data
      forecastSMAlist = forecast(modelSMA, h = timeSteps)
      
      # Get the forecast, high and low bounds returned by the SMA model
      forecastSMA = data.frame(Forecast = as.numeric(forecastSMAlist$mean), High = forecastSMAlist$upper, Low = forecastSMAlist$lower)
      # Fix the column names
      
      colnames(forecastSMA) = c('Forecast', 'High', 'Low')
      
      # Add NAs to the forecast to make the number of rows for data and forecast be equal which is required for plotting
      values$forecastSMA = rbind(data.frame(Forecast = rep(NA, length(dataSMA)), High = rep(NA, length(dataSMA)), Low = rep(NA, length(dataSMA))), forecastSMA)
      
      values$dataSMA = data.frame(Data = values$dataUser[,c(2)])
      
      # Format the table to be displayed
      values$tableSMA = data.frame(Date = values$dataUser[, c(1)], Data = values$dataUser[, c(2)], Forecast = values$forecastSMA$Forecast)
      
      # Set the errors to NA becuase forecast is beyond the data horizon
      values$MAPE_SMA = mean(abs((values$dataSMA$Data-values$forecastSMA$Forecast)/values$dataSMA$Data), na.rm = T) * 100
      values$RMSE_SMA = sqrt(mean((values$forecastSMA$Forecast - values$dataSMA$Data)^2, na.rm = T))
      
      
      
    } else {
      # Run this in case of time steps ahead forecast
      
      
      
      #### ARIMA ####
      
      
      # Convert the data column to time series
      if(input$f == '365' || input$f == '52' || input$f == '12'){
        dataARIMA = ts(values$dataUser[,c(2)], frequency = as.numeric(input$f))
      } else{
        dataARIMA = ts(values$dataUser[,c(2)])
      }
      
      # Fit the model
      if(input$fh == TRUE){
        modelARIMA = auto.arima(dataARIMA, seasonal = TRUE, lambda = 0)
      } else{
        modelARIMA = auto.arima(dataARIMA, lambda = 0)
      }
      
      # Get the number of time steps input from the user
      values$timeSteps = input$nTimeSteps
      
      # Forecast the said number of time steps ahead
      forecastARIMAlist = forecast::forecast(modelARIMA, h=values$timeSteps, level = c(95))
      
      # Get the forecast, high and low bounds returned by the ARIMA model
      forecastARIMA = data.frame(Forecast = as.numeric(forecastARIMAlist$mean), High = forecastARIMAlist$upper, Low = forecastARIMAlist$lower)
      colnames(forecastARIMA) = c('Forecast', 'High', 'Low')
      
      # Add NAs to the data and forecast to make the number of rows for data and forecast be equal which is required for plotting
      values$dataARIMA = data.frame(Data = c(values$dataUser[,c(2)], rep(NA, values$timeSteps)))
      values$forecastARIMA = rbind(data.frame(Forecast = rep(NA, nrow(values$dataUser)), High = rep(NA, nrow(values$dataUser)), Low = rep(NA, nrow(values$dataUser))), forecastARIMA)
      
      # Format the table to be displayed
      values$tableARIMA = data.frame(Date = c(values$dataUser[, c(1)], rep(NA, values$timeSteps)), Data = c(values$dataUser[, c(2)], rep(NA, values$timeSteps)), Forecast = values$forecastARIMA$Forecast)
      
      # Set the errors to NA becuase forecast is beyond the data horizon
      values$MAPE_ARIMA = NA
      values$RMSE_ARIMA = NA
      
      
      
      
      
      #### ETS ####
      
      
      # Convert the data column to time series
      if(input$f == '365' || input$f == '52' || input$f == '12'){
        dataETS = ts(values$dataUser[,c(2)], frequency = as.numeric(input$f))
      } else{
        dataETS = ts(values$dataUser[,c(2)])
      }
      
      # Fit the model
      modelETS = ets(dataETS)
      
      # Get the number of time steps input from the user
      values$timeSteps = input$nTimeSteps
      
      # Forecast the said number of time steps ahead
      forecastETSlist = forecast::forecast(modelETS, h = values$timeSteps, level = c(95))
      
      # Get the forecast, high and low bounds returned by the ETS model
      forecastETS = data.frame(Forecast = as.numeric(forecastETSlist$mean), High = forecastETSlist$upper, Low = forecastETSlist$lower)
      colnames(forecastETS) = c('Forecast', 'High', 'Low')
      
      # Add NAs to the data and forecast to make the number of rows for data and forecast be equal which is required for plotting
      values$dataETS = data.frame(Data = c(values$dataUser[,c(2)], rep(NA, values$timeSteps)))
      values$forecastETS = rbind(data.frame(Forecast = rep(NA, nrow(values$dataUser)), High = rep(NA, nrow(values$dataUser)), Low = rep(NA, nrow(values$dataUser))), forecastETS)
      
      # Format the table to be displayed
      values$tableETS = data.frame(Date = c(values$dataUser[, c(1)], rep(NA, values$timeSteps)), Data = c(values$dataUser[, c(2)], rep(NA, values$timeSteps)), Forecast = values$forecastETS$Forecast)
      
      # Set the errors to NA becuase forecast is beyond the data horizon
      values$MAPE_ETS = NA
      values$RMSE_ETS = NA
      
      
      
      
      #### TBATS ####
      
      
      # Convert the data column to time series
      if(input$f == '365' || input$f == '52' || input$f == '12'){
        dataTBATS = ts(values$dataUser[,c(2)], frequency = as.numeric(input$f))
      } else{
        dataTBATS = ts(values$dataUser[,c(2)])
      }
      
      # Fit the model
      modelTBATS = tbats(dataTBATS)
      
      # Get the number of time steps input from the user
      values$timeSteps = input$nTimeSteps
      
      # Forecast the said number of time steps ahead
      forecastTBATSlist = forecast::forecast(modelTBATS, h = values$timeSteps, level = c(95))
      
      # Get the forecast, high and low bounds returned by the TBATS model
      forecastTBATS = data.frame(Forecast = as.numeric(forecastTBATSlist$mean), High = forecastTBATSlist$upper, Low = forecastTBATSlist$lower)
      colnames(forecastTBATS) = c('Forecast', 'High', 'Low')
      
      # Add NAs to the data and forecast to make the number of rows for data and forecast be equal which is required for plotting
      values$dataTBATS = data.frame(Data = c(values$dataUser[,c(2)], rep(NA, values$timeSteps)))
      values$forecastTBATS = rbind(data.frame(Forecast = rep(NA, nrow(values$dataUser)), High = rep(NA, nrow(values$dataUser)), Low = rep(NA, nrow(values$dataUser))), forecastTBATS)
      
      # Format the table to be displayed
      values$tableTBATS = data.frame(Date = c(values$dataUser[, c(1)], rep(NA, values$timeSteps)), Data = c(values$dataUser[, c(2)], rep(NA, values$timeSteps)), Forecast = values$forecastTBATS$Forecast)
      
      # Set the errors to NA becuase forecast is beyond the data horizon
      values$MAPE_TBATS = NA
      values$RMSE_TBATS = NA
      
      
      
      
      
      
      #### SMA ####
      
      
      # Convert the data column to time series
      if(input$f == '365' || input$f == '52' || input$f == '12'){
        dataSMA = ts(values$dataUser[,c(2)], frequency = as.numeric(input$f))
      } else{
        dataSMA = ts(values$dataUser[,c(2)])
      }
      
      # Get the number of time steps input from the user
      values$timeSteps = input$nTimeSteps
      
      # Fit the model
      modelSMA = sma(dataSMA, h = values$timeSteps)
      
      # Forecast the said number of time steps ahead
      forecastSMAlist = forecast(modelSMA, h = values$timeSteps)
      
      # Get the forecast, high and low bounds returned by the SMA model
      forecastSMA = data.frame(Forecast = as.numeric(forecastSMAlist$mean), High = forecastSMAlist$upper, Low = forecastSMAlist$lower)
      colnames(forecastSMA) = c('Forecast', 'High', 'Low')
      
      # Add NAs to the data and forecast to make the number of rows for data and forecast be equal which is required for plotting
      values$dataSMA = data.frame(Data = c(values$dataUser[,c(2)], rep(NA, values$timeSteps)))
      values$forecastSMA = rbind(data.frame(Forecast = rep(NA, nrow(values$dataUser)), High = rep(NA, nrow(values$dataUser)), Low = rep(NA, nrow(values$dataUser))), forecastSMA)
      
      # Format the table to be displayed
      values$tableSMA = data.frame(Date = c(values$dataUser[, c(1)], rep(NA, values$timeSteps)), Data = c(values$dataUser[, c(2)], rep(NA, values$timeSteps)), Forecast = values$forecastSMA$Forecast)
      
      # Set the errors to NA becuase forecast is beyond the data horizon
      values$MAPE_SMA = NA
      values$RMSE_SMA = NA
      
      
      
    }
    
    # Alert the user that the forecast has been generated
    sendSweetAlert(session, 'Forecast Generated', 'Have a look!', type = 'success', btn_labels = 'View')
  })
  
  
  
  #### ARIMA Functions ####
  
  
  # Display the ARIMA table
  output$tableARIMA <- renderTable({
    req(values$tableARIMA)
    
    tableToDisplay = values$tableARIMA
    tableToDisplay[, 1] = format(tableToDisplay[, 1], '%d/%m/%Y')
    tableToDisplay
  })
  
  # Display the ARIMA plot
  output$plotARIMA <- renderPlotly({
    
    req(values$forecastARIMA)
    
    plot_ly(values$forecastARIMA, x=c(1:nrow(values$forecastARIMA))) %>%
      add_trace(y = values$dataARIMA$Data, type = 'scatter', mode = 'Lines', name = 'Data') %>%
      add_trace(y = ~High, type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                showlegend = FALSE, name = 'High') %>% 
      add_trace(y = ~Low, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                showlegend = FALSE, name = 'Low') %>%
      add_trace(y = ~Forecast, type = 'scatter', mode = 'lines',
                line = list(color='rgb(0,100,80)'),
                name = 'Forecast') 
  })
  
  # Display the ARIMA MAPE
  output$MAPE_ARIMA <- renderText({
    req(values$MAPE_ARIMA)
    paste('MAPE:', values$MAPE_ARIMA)
  })
  
  # Display the ARIMA RMSE
  output$RMSE_ARIMA <- renderText({
    req(values$RMSE_ARIMA)
    paste('RMSE:', values$RMSE_ARIMA)
  })
  
  # Download the ARIMA table as an excel spreadsheet
  output$downloadARIMA <- downloadHandler(
    filename = function(){
      ifelse(input$filenameARIMA == '', 'ARIMA.xlsx', paste0(input$filenameARIMA, '.xlsx'))
    },
    content = function(file){
      tableToWrite = values$tableARIMA
      tableToWrite[,1] = format(tableToWrite[,1], "%d/%m/%Y")
      write_xlsx(tableToWrite, file)
    }
  )
  
  
  
  #### ETS Functions ####
  
  
  # Display the ETS table
  output$tableETS <- renderTable({
    req(values$tableETS)
    
    tableToDisplay = values$tableETS
    tableToDisplay[, 1] = format(tableToDisplay[, 1], '%d/%m/%Y')
    tableToDisplay
  })
  
  # Display the ETS Plot
  output$plotETS <- renderPlotly({
    req(values$forecastETS)
    
    plot_ly(values$forecastETS, x=c(1:nrow(values$forecastETS))) %>%
      add_trace(y = values$dataETS$Data, type = 'scatter', mode = 'Lines', name = 'Data') %>%
      add_trace(y = ~High, type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                showlegend = FALSE, name = 'High') %>% 
      add_trace(y = ~Low, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                showlegend = FALSE, name = 'Low') %>%
      add_trace(y = ~Forecast, type = 'scatter', mode = 'lines',
                line = list(color='rgb(0,100,80)'),
                name = 'Forecast') 
  })
  
  # Display the ETS MAPE
  output$MAPE_ETS <- renderText({
    req(values$MAPE_ETS)
    paste('MAPE:', values$MAPE_ETS)
  })
  
  # Display the ETS RMSE
  output$RMSE_ETS <- renderText({
    req(values$RMSE_ETS)
    paste('RMSE:', values$RMSE_ETS)
  })
  
  # Download the ETS table as an excel spreadsheet
  output$downloadETS <- downloadHandler(
    filename = function(){
      ifelse(input$filenameETS == '', 'ETS.xlsx', paste0(input$filenameETS, '.xlsx'))
    },
    content = function(file){
      tableToWrite = values$tableETS
      tableToWrite[,1] = format(tableToWrite[,1], "%d/%m/%Y")
      write_xlsx(tableToWrite, file)
    }
  )
  
  
  
  #### TBATS Functions ####
  
  
  # Display the TBATS table
  output$tableTBATS <- renderTable({
    req(values$tableTBATS)
    
    tableToDisplay = values$tableTBATS
    tableToDisplay[, 1] = format(tableToDisplay[, 1], '%d/%m/%Y')
    tableToDisplay
  })
  
  # Display the TBATS Plot
  output$plotTBATS <- renderPlotly({
    req(values$forecastTBATS)
    
    plot_ly(values$forecastTBATS, x=c(1:nrow(values$forecastTBATS))) %>%
      add_trace(y = values$dataTBATS$Data, type = 'scatter', mode = 'Lines', name = 'Data') %>%
      add_trace(y = ~High, type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                showlegend = FALSE, name = 'High') %>% 
      add_trace(y = ~Low, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                showlegend = FALSE, name = 'Low') %>%
      add_trace(y = ~Forecast, type = 'scatter', mode = 'lines',
                line = list(color='rgb(0,100,80)'),
                name = 'Forecast') 
  })
  
  # Display the TBATS MAPE
  output$MAPE_TBATS <- renderText({
    req(values$MAPE_TBATS)
    paste('MAPE:', values$MAPE_TBATS)
  })
  
  # Display the TBATS RMSE
  output$RMSE_TBATS <- renderText({
    req(values$RMSE_TBATS)
    paste('RMSE:', values$RMSE_TBATS)
  })
  
  # Download the TBATS table as an excel spreadsheet
  output$downloadTBATS <- downloadHandler(
    filename = function(){
      ifelse(input$filenameTBATS == '', 'TBATS.xlsx', paste0(input$filenameTBATS, '.xlsx'))
    },
    content = function(file){
      tableToWrite = values$tableTBATS
      tableToWrite[,1] = format(tableToWrite[,1], "%d/%m/%Y")
      write_xlsx(tableToWrite, file)
    }
  )
  
  
  
  
  #### SMA Functions ####
  
  
  # Display the SMA table
  output$tableSMA <- renderTable({
    req(values$tableSMA)
    
    tableToDisplay = values$tableSMA
    tableToDisplay[, 1] = format(tableToDisplay[, 1], '%d/%m/%Y')
    tableToDisplay
  })
  
  # Display the SMA Plot
  output$plotSMA <- renderPlotly({
    req(values$forecastSMA)
    
    plot_ly(values$forecastSMA, x=c(1:nrow(values$forecastSMA))) %>%
      add_trace(y = values$dataSMA$Data, type = 'scatter', mode = 'Lines', name = 'Data') %>%
      add_trace(y = ~High, type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                showlegend = FALSE, name = 'High') %>% 
      add_trace(y = ~Low, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                showlegend = FALSE, name = 'Low') %>%
      add_trace(y = ~Forecast, type = 'scatter', mode = 'lines',
                line = list(color='rgb(0,100,80)'),
                name = 'Forecast') 
  })
  
  # Display the SMA MAPE
  output$MAPE_SMA <- renderText({
    req(values$MAPE_SMA)
    paste('MAPE:', values$MAPE_SMA)
  })
  
  # Display the SMA RMSE
  output$RMSE_SMA <- renderText({
    req(values$RMSE_SMA)
    paste('RMSE:', values$RMSE_SMA)
  })
  
  # Download the SMA table as an excel spreadsheet
  output$downloadSMA <- downloadHandler(
    filename = function(){
      ifelse(input$filenameSMA == '', 'SMA.xlsx', paste0(input$filenameSMA, '.xlsx'))
    },
    content = function(file){
      tableToWrite = values$tableSMA
      tableToWrite[,1] = format(tableToWrite[,1], "%d/%m/%Y")
      write_xlsx(tableToWrite, file)
    }
  )
  
  
}

shinyApp(ui, server)