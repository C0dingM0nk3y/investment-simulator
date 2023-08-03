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
library(tidyr)
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
    title = "Select one Investment Strategy:",
    
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
  
  hr(),
  
  fluidRow(align="center",
           column(3, #offset = 1,
                  inputPanel(
                    h3("How to use this simulator"),
                    actionButton("runAnalysis", "Simulate")
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
                  checkboxInput("infl_correction",
                                "Correct for inflation?", value = FALSE
                                ),
                  p(em("Purchasing power of ",
                      # textOutput(renderText(input$monthly_inv), inline = T),
                       "$ was much higher back then then it is now. Tick the box to correct for inflation."))
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
      
      plotOutput("plot"),
      plotOutput("hist"),
      plotOutput("pnl"),
      plotOutput("asset"),
      
      tableOutput("settings"),
      tableOutput("endopoints"),
      
      dataTableOutput("table"),
    ),
  ),
  ),
  
  # REBAL SIMULATOR ####
  tabPanel(title=strong("Rebalancing Simulator"),
           fluidRow(align="center",
                    column(12,
                          h2("[COMING SOON]"),
                           ),
                    ),
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