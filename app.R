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

# 

# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "investment-simulator",
  
  fluidRow(
    column(3, offset = 1,
           h1("somehting catchy"),
           ),
    column(8,
           h4("Chose one day in the past. Starting from that day, simualte the effecto of buying X amount of Y, every 30 days. How much would it be worth now?"),
           ),
  ),

  sidebarLayout(
    sidebarPanel(
      h2("Step 1"),
      h3("Simulation Duration"),
    ),
    mainPanel(
      sliderInput("years",
                  "How long ago should the investment start? (years)",
                  value = 10, min = 1, max = 40, step = 1, ticks = FALSE),
      p(em("you do not know from which date to start? Try today, 10 years ago. Or your 25th birthday.")),
    ),
  ),
  
  sidebarLayout(
    sidebarPanel(
      h2("Step 2"),
      h3("Investment Amount"),
    ),
    mainPanel(
      numericInput("monthly_inv",
                   "How much was invested (per month)?",
                   value = 100, min = 10, step = 100),
    ),
  ),
  
  sidebarLayout(
    sidebarPanel(
      h2("Step 3"),
      h3("Select Investment"),
    ),
    mainPanel(
      textInput("symbol",
                p("Chose one asset to invest into", br(), em("default: SPY = 'S&P500 full index'")), 
                value = "SPY", placeholder = "any asset name supported by Yahoo Finance"),
      p(em("For a guide on Stock and Index names, refer to ", a("Yahoo Finance Symbol List", href="https://finance.yahoo.com/lookup/?guccounter=1", )),
      ),
    ),
  ),
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  symbol <- "SPY"
  startdate <- "1980-01-01" %>% as.Date()
  inv_qnt <- 100
  
  simulateInv <- function(symbol, startdate, inv_qnt){
    # DOWNLOAD LATEST DATA FROM YAHOO
    xts <- getSymbols(symbol, 
                       from = startdate,
                       auto.assign = FALSE) #required to assign to custom variable name
    
    # CALCULATE AVERAGE PRICE 
    # removes symbol name from df headers #eg. from SPY.Open to SPY
    df <- data.frame(xts) #converts from xts object to standard df
    colnames(df) %<>% str_remove(symbol)  %>% 
      str_remove(".")
    
    df[,"avg_price"] <- (df$Open + df$Close)/2 # average price (mean of OPEN and CLOSE price)
    
    # CREATE SUBSET: 1 INVESTMENT EVERY 30 days

    #> Starting from startdate, subset table to only show prices on the days where a new investment was done.
    #> NB: market is closed on SAT-SUN. Therefore the market data are missing all the rows corresponging to holidays.
    #> For this reason, 1 investment every 30 days is converted into 1 investment every 20 work-days.
    #> Table is filtered so to only keep one row every 20, starting from startdate
    
    df[,"daysFromStart"] <- 1:nrow(df)-1
    df[,"filter"] <- ifelse(df$daysFromStart %% 20 ==0, TRUE, FALSE)
    
    ss <- subset(df, filter == TRUE, select = c("avg_price", "daysFromStart"))
    
    # SIMULATE PURCHASE
    ss[,"value_inv"] <- inv_qnt
    ss[,"buy_qnt"] <- with(ss, value_inv/avg_price)
    
    # cumulative calc
    ss[,"cum_inv"] <- cumsum(ss$value_inv)
    ss[,"cum_qnt"] <- cumsum(ss$buy_qnt)
    
    # current value of investment
    latestPrice <- tail(df$avg_price, 1)
    ss[,"cum_value"] <- ss$cum_qnt*latestPrice
    
    # ROI
    ss[,"ratioTEMP"] <- with(ss, cum_value/cum_inv)
    ss[,"ROI"] <- with(ss, (cum_value-cum_inv)/cum_inv)
    
    # STOPPED HERE
    time <- difftime(tail(row.names(df), 1), head(row.names(df), 1), units = "weeks") %>%
      as.numeric()/52 
    finalRatio <- tail(ss$ratioTEMP, 1)
    
    # ORGANIZE RESULTS
    res <- ss
    
    return(res)
  }
  
  DF <- simulateInv(symbol, startdate, inv_qnt)


}

# Run the application 
shinyApp(ui = ui, server = server)
