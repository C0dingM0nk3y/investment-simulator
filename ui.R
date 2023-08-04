#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
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
ui <- fluidPage(theme = shinytheme("darkly"),
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
  navbarPage(theme = shinytheme("darkly"),
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
                         h4(span(style="color:red", u("Challenge:")),
                            em("can you find one investment that did NOT produce income, after >20 years? ;)"),
                         
               ),
             ),
             
             ),
  
  hr(),
  
      sidebarLayout(

               sidebarPanel(align="center", width = 3, #offset = 1,
                      h4("Examples of common investments:"),
                      #div(style="background:white; color:black; font-size:75%; width:70%",
                      div(align="left", style="font-size:80%; width:100%",
                        em(
                        strong("^GSPC"), "=  S&P500 (Index)",br(),
                        strong("^IXIC"), "=  NASDAQ (Index)",br(),
                        strong("^DJI"), "=  Dow Jones Industrial Average (Index)",br(),
                        strong("^TNX"), "=  Treasury Yield 10 years (Index)",br(),
                        strong("^ERIX"), "=  European Revewable Energy Total (Index)",br(),br(),
                        strong("SPY"), "=  S&P500 Index Tracker (ETF)",br(),
                        strong("VTI"), "=  Vangard Total Stock (ETF)",br(),
                        strong("EEM"), "=  Emerging Markets (ETF)",br(),
                        strong("EWI"), "=  Italian Market (ETF)",br(),br(),
                        strong("GOOG"), "=  Google (Stock)",br(),
                        strong("AAPL"), "=  Apple (Stock)",br(),br(),
                        strong("BTC-USD"), "=  Bitcoin (Crypto)",br(),
                        strong("ETH-USD"), "=  Ethereum (Crypto)",br(),
                        )
                        ),
               ),
        
               mainPanel(width = 9,
                 fluidRow(align="center",
                   column(6, #offset = 1,
                          h3("1. Find Yahoo Finance tracker"),
                          
                          p("Use",
                            a("Yahoo Finance Search Engine (link)", href="https://finance.yahoo.com/lookup/?guccounter=1"),
                            "to find a list supported symbols names"),
                          p("Retrieve the correct",code("Symbol name"), "from Yahoo website and paste it below:"),         
                          
                          # split columns in 2 parts (to align search box to search button)
                          fluidRow(
                            column(6, offset=2, textInput("symbol", label="", value = "SPY",  width = "100%",
                                                          placeholder = "Symbol Name"),),
                            column(3, align="left", br(), #h3() is empty, as spacer 
                                   actionButton("symbolsubmit", label = "Search", width = "100%"),
                            ),
                          ),
                          
                          hr(),
                          
                          # split columns in 2 parts (to align search box to search button)
                          h3("2. Investment Amount"),
                                   
                          numericInput("monthly_inv",
                                      "Monthly Investment:",
                                      value = 100, min = 0, step = 100),
                                                    
                          ), 
                   column(6, #offset = 1,
                          
                          h3("3. Investment Start Date"),
                          plotOutput("market", height = "150px", width = "90%"),
                          uiOutput("ui_startDate"),
                          p("Investment duration:", strong(textOutput("duration", inline = T))),
                          hr(),
                          h3("4. Inflation Correction"),
                          div(style="width:75%",
                           em("Purchasing power of $ was higher in the past then it is now. Tick to correct for inflation.")),
                          checkboxInput("infl_correction",
                                        span("Apply Inflation Correction?", style="color:orange"), value = FALSE),
                   ),
                 ),
               ),   
      ),

  hr(),
  
  sidebarLayout(
    sidebarPanel(align="center",
      h3("Simulation Data"),
      hr(),
      tableOutput("settings"),
      ),

    mainPanel(
      fluidRow(align="center",
               column(6,
                      h3("Results:", actionButton("runAnalysis", strong("Click to Refresh"), inline=TRUE)),
                      hr(),
                      tableOutput("endopoints"),
               ),
               column(6,
                      plotOutput("end_plot", height = "300px", width = "90%"),
               ),
      ),
    ),
  ),
    
    
  hr(),
  h1("Investment breakdown [WIP: Aesthetics will be fixed soon]"),
  
  sidebarLayout(
      sidebarPanel(
        h4("Comparison: invested vs. value"),
      ),
      mainPanel(
        plotOutput("hist"),
      )
    ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Profit and Losses (PNL)"),
    ),
    mainPanel(
      plotOutput("pnl"),
    )
  ),
  
  sidebarLayout(
    sidebarPanel( 
      h4("Asset Accumulation"),),
    mainPanel(
      plotOutput("asset"),
                          ),
  ),
  
  #sidebarLayout(sidebarPanel( h4("FULL DATA"),),
  #    mainPanel(dataTableOutput("table"),),
  #  ),
  
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