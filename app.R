library(shiny)
library(quantmod)
ui <- fluidPage(
  
  titlePanel("Shiny Stock Chart"),
  sidebarLayout(
    sidebarPanel(
      selectInput("symb","Symbol",c("YAHOO"="YHOO","GOOGLE"="GOOG","APPLE"="AAPL")),
      #submitButton("go"),
      dateRangeInput(inputId = "daterange", "Date Range", start=Sys.Date()-120, end=Sys.Date()),
      checkboxInput(inputId = "sma10", label = "SMA 10"),
      checkboxInput(inputId = "sma50", label = "SMA 50"),
      checkboxInput(inputId = "ema10", label = "EMA 10"),
      checkboxInput(inputId = "ema50", label = "EMA 50"),
      checkboxInput(inputId = "bbands", label = "Bolinger Bands"),
      checkboxInput(inputId = "rsi", label = "RSI"),
      checkboxInput(inputId = "macd", label = "MACD")
      
    ),
    mainPanel(
      plotOutput("barchart")
    )
  )
  
)

server <- function(input, output){

   stockData <- reactive({
     message(paste("Getting Symbol ",input$symb))
     getSymbols(input$symb, src="yahoo", auto.assign = FALSE, 
                from=format(input$daterange[1]), 
                to=format(input$daterange[2]))
   }) 
     
  output$barchart <- renderPlot({    
    message("Replotting...")
    TA <- "addVo()"
    if(input$sma10){ TA <- paste(TA, "addSMA(10)", sep=";") }
    if(input$sma50){ TA <- paste(TA, "addSMA(50)", sep=";") }
    if(input$ema10){ TA <- paste(TA, "addEMA(10)", sep=";") }
    if(input$ema50){ TA <- paste(TA, "addEMA(50)", sep=";") }
    if(input$rsi){ TA <- paste(TA, "addRSI()", sep=";") }
    if(input$bbands){ TA <- paste(TA, "addBBands()", sep=";") }
    if(input$macd){ TA <- paste(TA, "addMACD()", sep=";") }
    chartSeries(stockData(), theme="white", TA=TA)
  })
  
}


shinyApp(ui = ui, server = server)




