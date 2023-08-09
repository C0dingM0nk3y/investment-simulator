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

textcol <- function(text, color="#84b0fa"){ #quick formatting "#375a7f"
  underlinedText <- span(style=paste0("color:",color),
                         text)
  return(underlinedText)
}

# UI ####
ui <- fluidPage(theme = shinytheme("darkly"),
  title = "investment-simulator",
  
  br(), #black line on top
  # TITLE ####
    sidebarLayout(
      mainPanel(align= "center", width = 6, 
                h2("investment-simulator_v0"),
                p(em("preliminary release: 2023.08.05")),
                hr(),
                h4("What does it do?") %>% u(),
                p("This script use ",u("historical data"), "from different investments, to simulate the purchase of a ", u("fixed value"), "of the ", u("defined asset, every 30 days"), "starting from the", u("start date"), "until today(*)."),
                br(),
                p(textcol(color="orange", 
                          em("*this strategy is called DCA. Scroll to the bottom to find a description and some references")),
                  ),
                ),
      sidebarPanel(align = "center", width = 5,
                  h4("What to do?") %>% u(),
                  p("Choose ",textcol("One asset"),", then hit 'Search'", br(),
                    
                    "Select a ",textcol("Date in the past")," and an ",textcol("Amount to invest"),".",br(),
                    
                    textcol("Change the parameters dynamically"), "and test their influence on the final yield."
                    ),
                  
                  br(),
                  p(em("How much would you have earned if you did so?")),
                  p(em("When was the best time to begin investing?")),
         ),
    ),

  # NAVBAR ####
  navbarPage(theme = shinytheme("darkly"),
    title = "Select one Investment Strategy:",
    
    # DCA SIMULATOR ####
    tabPanel(title=strong("DCA simulator"),
             
             #> INTRO ####
             sidebarLayout(
               sidebarPanel(width = 3, align="center",
                 h3("Chose on Asset from Yahoo Finance"),
                 p("Historical data is extracted from ", a("Yahoo Finance", href="https://finance.yahoo.com/lookup/?guccounter=1"),),
                 
               ),
               mainPanel(width=9,
                         fluidRow(
                           column(7, align="center",
                                  h3("1. Find Yahoo Finance tracker"),
                                  p("Refer to",
                                    a("Yahoo Finance Search Engine (link)", href="https://finance.yahoo.com/lookup/?guccounter=1"),
                                    "to find the ",code("Symbol.name"), "for the investment of your choice."),
                                  
                                  # split columns in 2 parts (to align search box to search button)
                                  fluidRow(
                                    column(6, offset=2, textInput("symbol", label="Paste it below and hit 'Search'", value = "SPY",  width = "100%",
                                                                  placeholder = "Symbol Name"),),
                                    column(3, align="left", br(), #h3() is empty, as spacer 
                                           actionButton("symbolsubmit", label = "Search", width = "100%"),
                                    ),
                                  ),
                                  br(),
                                  p(em("1) some of Yahoo symbol names are different than those used elsewhere. Make sure to check the spelling on their website")),
                                  p(em("2) some symbol names include special char. (e.g. ^ for ^GSPC)")),
                                  ),
                           
                           column(5, align="center", style = 'border-left: 1px solid',
                                 h4("Common investments Symbols:"),
                                 
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
                                     ),
                                 ),
                                 ),
                           ),
                         ),
             ),
             
  hr(),
  
   fluidRow(align="center",
     column(4, #offset = 1,
            h4("2. Investment Start Date"),
            plotOutput("market", height = "150px", width = "90%"),
            uiOutput("ui_startDate"),
            p("Investment duration:", strong(textOutput("duration", inline = T))),
            hr(),
            numericInput("monthly_inv",
                         h4("3. Monthly Investment"),
                         value = 100, min = 0, step = 100),
                                      
            ), 
     column(8, #offset = 1,
            sidebarPanel(width=12,
              h3("Simulation Results"),
              hr(),
              fluidRow(
                column(6, 
                       tableOutput("endopoints"),
                       hr(),
                       checkboxInput("infl_correction",
                                     span("Correct for inflation?", style="color:orange"), value = FALSE),
                       div(style="width:80%",
                        em("Purchasing power of $ was higher in the past then it is now. Tick to correct for inflation.")),
                       ),
                column(6,
                       plotOutput("end_plot", height = "300px", width = "90%"),
                       ),
                
              ),
            ),
            
     ),
   ),

  hr(),
  div(align="center", 
      h1("Investment breakdown"),
      h4(style="color:orange", em("WIP: A mode detailed description of those plots will follow soon"),),),
  
  fluidRow(align="center",
    column(4,
           h4("Comparison: invested vs. value"),
           plotOutput("hist", height = "300px"),
           ),
    column(4, style = 'border-left: 1px solid',
           h4("Profit and Losses (PNL)"),
           plotOutput("pnl", height = "300px"),
           ),
    column(4, style = 'border-left: 1px solid',
           h4("Asset Accumulation (for each year)"),
           plotOutput("asset", height = "300px"),
           ),
  ),
  hr(),
  
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
  
  #dataTableOutput("table")
  ),
  
  # REBAL SIMULATOR ####
  tabPanel(title=strong("Rebalancing Simulator"),
               fluidRow(align="center",
                    column(12,
                          h2("[COMING SOON]"),
                           ),
                    ),
  ),
  
  # TEXT REPOSITORY ####
  tabPanel(title=strong("About this Project"),
           
             sidebarPanel(align = "center", width=12,
                       p(em(
                          "Many people say to invest early and benefit long term.",br(),
                          "Yet many others seem to have lost so much with their invesments, and they regret they have taken that decision.",br(),
                          "Who to trust, then?"),
                        p("As a Data Scientist, I invite you ", u("to trust no one"), ".", br(), 
                          "Instead, have a look at what the data suggest, and draw your own conclusions."),),
                ),
           
           sidebarLayout(
             sidebarPanel(width = 4,
                       h4("Project Aim"),
             ),
             mainPanel(align = "center", width = 8,
                          p("I wrote this tool for those young/new investors who are worried to lose money because of market fluctuations, and therefore miss on precious opportunities to invest their money. Worse, they attempt to time the market(with little experience), growing anxious about thier choices and end up losing sleep and money through trading."),
                          p("I hope this tool will help someone to grow more confidence into this process, and and begin their path toward financial freedom"),
             ),
           ),
           
           sidebarLayout(
             sidebarPanel(
               h4("About DCA"), 
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
             sidebarPanel(h4("About investing")),
             mainPanel(align = "center",
                        p("add this note somewhere: when used in combination with a ETF (aka, investing on the whole market at ONCE), DCA are one of the safest and more reliable sources of passive income.",
                          a(href="https://www.investopedia.com/terms/e/etf.asp", "(what is a ETF?)"),
                        ),
                        
                        p(em("*all it take is to do some reserch to select A DIVERSIFIED PANEL OF ASSETS that is matching with our financial goals, risk tolerance, and desire to impact future economy. This should be discussed with a financial advisor, to make sure to make sound choices. After that, it is enough to set-up a recurring transaction and... that is it, actually.")),
                        p("DCA are one of the most simple investment strategy and one that requires little maintenance, while still providing with a reasonably safe source of extra incomes.",
                          "Yet, few people use them. Mainly, becuase of their fear of losing money during hte (inevitable!) phases of drowdown. This this tool, I hope people will be able to play around, and discover themselves that not only the drowdown phases are a physiological part of the process, and should not be feared. But actually those are the phases where real gain are done",
                          "If you are new to investing, you can use this tool to:")
                 ),
           ),
  ),
  ),
  
  # Credits ####
  hr(),
  div(align="center",
      p("developed by:", a("C0dingM0nk3y", href="https://github.com/C0dingM0nk3y/"),
       ":|: source code available on GitHub:", a("investment-simulator", href="https://github.com/C0dingM0nk3y/investment-simulator"),
      ":|: Check out my repository for other tools for financial education", em("(coming soon)"))),
  hr(),
)