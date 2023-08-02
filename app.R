#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quantmod)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

u <- function(text){ #quick formatting
  underlinedText <- span(style="text-decoration:underline",
                         text)
  return(underlinedText)
}

# 

# UI ####
ui <- fluidPage(
  title = "investment-simulator",
  
  # TITLE ####
  fluidRow(
    column(8, offset = 2, align = "center",
           h1("investment-simulator"),
           p("This small project came from the desire to help people understanding better why it is important to invest their money and why -despite there are some risks associated to investing- sticking to healthy investment strategies (like a simple DCA, see below), often produce a safe and reliable results despite requiring so little effort*, that even a simple computer program may take advantage of it."),
 
           p(em(
             "Many people say to invest early and benefit long term.",br(),
             "Yet many others seem to have lost so much with their invesments, and they regret they have taken that decision.",br(),
           "Who to trust, then?"),
           p("As a Data Scientist, I invite you ", u("to trust no one"), ".", br(), 
               "Instead, have a look at what the data suggest, and draw your own conclusions."),),
  ),
  ),
  
  # NAVBAR ####
  navbarPage(
    title = "Select one tool:",
    
    # DCA SIMULATOR ####
    tabPanel(title=strong("DCA simulator"),
             
             #> INTRO ####
             sidebarLayout(
               sidebarPanel(
                 h3("What is Dollar Cost Averaging?"), 
               ),
               mainPanel(width=7,
                 p(strong("Dollar Cost Averaging (DCA) -"), 
                   a(href="https://en.wikipedia.org/wiki/Dollar_cost_averaging", "link to wikipedia"),
                   br(),
                   "DCA refers to the practice of purchasing a ",strong("fixed value"), "of a asset at a", strong("specific interval of time"), "and", strong("regardless of current market price"), br(),
                   em("For example, a simple DCA strategy may be to buy 100€ worth of an ETF tracking the S&P500 index every 30 days. This is the defoult setting for this web-tool."),
                 ),
                 p("DCA is one of the simpler-yet-effective LONG-TERM investment strategy. As there is ", u("no attempt to time the market"), "this strategy can be completely passive, and there is no need for the investor to costantly be updated on current market situation. By splitting the investment into many small instances, the investor is buying the asset on an average price, and minimizing the effect of market volatility. While there may be time at which the investment will seem to be in loss, keep buying at a discounted price will ensure an even higher profit on the subsequent expansion phase f the market."),
               ),
             ),
             
             hr(),
             
             sidebarLayout(
               sidebarPanel(
                 h3("How to use this tool?"), 
               ),
               mainPanel(align="center", width=7,
                         p(strong("DCA are very easy to implement and mantain."), 
                           "However, it is of foremost importance to", strong("carefully select the investment to which to commit to"), "and preferably do so ", u("with the assitance of a financial advisor"), "(of which, I am not)"),
                         p(strong("In fact, DCA are so simple that even a computer program can profit from using them!")),
                         #p("Try for yourself by using the tool below to see the effect of a DCA of differetn kind of assets."),
                         hr(),
                         h4("This webtool uses historical data from Yahoo Finance to simulate the effect of a DCA on most common investment options"),
                         h4(u("Challenge:"),em("can you find one that did NOT produce income, after >20 years? ;)"),
                         
               ),
             ),
             
             ),
  ),
  ),
  
  hr(),
  
  fluidRow(align="center",
           column(3, #offset = 1,
                  inputPanel(
                    h3("How to use this simulator"),
                    actionButton("runAnalysis", "Analyze")
                  ),
           ),
           
           column(4, #offset = 1,
                  h3("1. Select Investment"),
                  textInput("symbol",
                        p("Chose one asset to invest into", br(), em("default: SPY = 'S&P500 full index'")), 
                        value = "SPY", placeholder = "any asset name supported by Yahoo Finance", 
                        ),
              
                  p(em("For a guide on Stock and Index names, refer to ", 
                   a("Yahoo Finance Symbol List", href="https://finance.yahoo.com/lookup/?guccounter=1")),
                  ),
                  hr(),
                  
                  h3("2. Investment Amount"),
                  numericInput("monthly_inv",
                               "How much was invested (every MONTH)?",
                               value = 100, min = 0, step = 100
                   ),
            ),
           
           column(4, #offset = 1,
                  h3("3. Investment Start Date"),
                  plotOutput("market", height = "150px", width = "90%"),
                  uiOutput("ui_startDate"),
                  p("Investment duration:", strong(textOutput("duration", inline = T))),
                  p(em("you do not know from which date to start? Try today, 10 years ago. Or your 25th birthday.")),
           ),
          
    ),
  hr(),
  
  fluidRow(
    column(10, offset = 1,
      h1("some nice plot!"),
      dataTableOutput("table"),
    ),
  ),

  hr(),
  
  fluidRow(
    column(10, offset = 1, align = "center",
        p("add this note somewhere: when used in combination with a ETF (aka, investing on the whole market at ONCE), DCA are one of the safest and more reliable sources of passive income.",
        a(href="https://www.investopedia.com/terms/e/etf.asp", "(what is a ETF?)"),
        ),
           
        p(em("*all it take is to do some reserch to select A DIVERSIFIED PANEL OF ASSETS that is matching with our financial goals, risk tolerance, and desire to impact future economy. This should be discussed with a financial advisor, to make sure to make sound choices. After that, it is enough to set-up a recurring transaction and... that is it, actually.")),
    ),
  ),
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #USES DEFAULT INPUTS TO BUILD UI
  #symbol <- "SPY"
  #startdate <- "1980-01-01" %>% as.Date()
  #inv_qnt <- 100
  
  # OUTPUT: PLOT Market trends
  symbolData <- function(symbol, startdate){
    #> get data using quantmod::getSymbolData(), then 
    #> perform basic df tidying, like adding Date column and calculating AVG price

    # DOWNLOAD LATEST DATA FROM YAHOO
    xts <- getSymbols(symbol, 
                      from = startdate,
                      auto.assign = FALSE) #required to assign to custom variable name
    
    # CALCULATE AVERAGE PRICE 
    # removes symbol name from df headers #eg. from SPY.Open to SPY
    df <- data.frame(xts) #converts from xts object to standard df
    colnames(df) %<>% str_remove(symbol)  %>% #remove symbol names
      str_remove(".")
    
    df[,"Price_AVG"] <- (df$Open + df$Close)/2 # average price (mean of OPEN and CLOSE price)
    
    # ADD Posit Column (for plotting purposes)
    df[,"Date"] <- row.names(df) %>% as.POSIXct()
    
    return(df)
  }
  
  # CREATE UI ELEMENTS and set defaults INPUTS
  REACT <- reactiveValues() 
  
  # OUTPUT: UI + OUTPUT Market plot preview
  # this runs on start-up and after every user chance of asset

  observeEvent(input$symbol,
    {
      REACT$data_full <- symbolData(input$symbol, 
                                    startdate = "1950-01-01") #recovers the earliest available data
      
      REACT$minDate <- row.names(REACT$data_full) %>% 
        as.Date() %>% min()
    })
  
      
  # First, create the reactive slider UI (needed to get default input$startdate value
  output$ui_startDate <- renderUI(sliderInput(inputId = "startdate", label="Select Start Date",
                                              min = REACT$minDate, max=today()-366,
                                              value=REACT$minDate))
  
  # Second, render plot
  output$market <- renderPlot({
    ggplot(data = REACT$data_full) +
      geom_line(aes(x=Date, y=Price_AVG)) +
      geom_vline(xintercept = as.POSIXct(input$startdate), linetype=2, color="lightblue3", size =0.9) +
      annotate(geom="label", 
               label="Inv. Start", hjust=0, fill="lightblue",
               x=as.POSIXct(input$startdate+180), y=max(REACT$data_full$Price_AVG)*0.9) +
      theme_light()
      })
  
  
  # OUTPUT: TEXT (Investment Duration)
  output$duration <- renderText(
    difftime(today(), input$startdate, units = "weeks") %>% 
      divide_by(52) %>%
      floor() %>% 
      paste("years")
  )
  
  
  simulateInv <- function(symbol, startdate, inv_qnt){
    
    # RE-IMPORT asset data, starting from start date #FUTURE: this may be a filter set on REACT$data_full
    df <- symbolData(symbol, startdate)
    
    # CREATE SUBSET: 1 INVESTMENT EVERY 30 days
    
    #> Starting from startdate, subset table to only show prices on the days where a new investment was done.
    #> NB: market is closed on SAT-SUN. Therefore the market data are missing all the rows corresponging to holidays.
    #> For this reason, 1 investment every 30 days is converted into 1 investment every 20 work-days.
    #> Table is filtered so to only keep one row every 20, starting from startdate
    
    df[,"daysFromStart"] <- 1:nrow(df)-1
    df[,"filter"] <- ifelse(df$daysFromStart %% 20 ==0, TRUE, FALSE)
    
    ss <- subset(df, filter == TRUE, select = c("Price_AVG", "Adjusted", "daysFromStart"))
    
    # SIMULATE PURCHASE
    ss[,"value_inv"] <- inv_qnt
    ss[,"buy_qnt"] <- with(ss, value_inv/Price_AVG)
    
    # cumulative calc
    ss[,"cum_inv"] <- cumsum(ss$value_inv)
    ss[,"cum_qnt"] <- cumsum(ss$buy_qnt)
    
    # current value of investment
    latestPrice <- tail(df$Price_AVG, 1)
    ss[,"cum_value"] <- ss$cum_qnt*latestPrice
    
    # ROI
    ss[,"ratioTEMP"] <- with(ss, cum_value/cum_inv)
    ss[,"ROI"] <- with(ss, (cum_value-cum_inv)/cum_inv)
    
    # STOPPED HERE
    time <- difftime(tail(row.names(df), 1), head(row.names(df), 1), units = "weeks") %>%
      as.numeric()/52 
    finalRatio <- tail(ss$ratioTEMP, 1)
    
    #ADJCALC
    ss[,"buy_adj"] <- with(ss, value_inv/Adjusted)
    ss[,"cum_adj"] <- cumsum(ss$buy_adj)
    ss[,"cum_adjVal"] <- ss$cum_adj*latestPrice
    ss[,"ROI_adj"] <- with(ss, (cum_adjVal-cum_inv)/cum_inv)
    ss[,"ratioTEMP_Adj"] <- with(ss, cum_adjVal/cum_inv)
    
    for (n in 1:30){
      print(paste(n , 1.06^n))
    }
    
    # ORGANIZE RESULTS
    res <- ss
    
    return(res)
  }
  
  # Update plots and tables             
  observeEvent(input$runAnalysis,
                 {
                   #isolate variable to avoid constant refreshing while user set parameters
                   REACT$simulation <- simulateInv(isolate(input$symbol), 
                                                   isolate(input$startdate), 
                                                   isolate(input$monthly_inv))
                   
                   output$table <- renderDataTable(
                     REACT$simulation
                   )
                 }
  )
  

}

# Run the application 
shinyApp(ui = ui, server = server)
