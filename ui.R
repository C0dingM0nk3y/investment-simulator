#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("_init.R")

# UI ####
ui <- fluidPage(theme = shinytheme("superhero"),
  # STYLE FUNCTIONS ####
  labelSuppress("symbol"),
  styleEdit("h4", "margin-top:0px"),
  styleEdit("#symbolsubmit", "background:#FFFFFF47"),
  #styleEdit("#infl_correction", "font-size:110%"),
  
  # TITLE ####
  title = "investment-simulator",
  br(), #black line on top
  
    sidebarLayout(
      mainPanel(align= "center", width = 4, 
                hr(),
                h2("investment-simulator_v1"),
                p(em("by:", 
                     a("C0dingM0nk3y", href="https://github.com/C0dingM0nk3y/"),
                     "released: 2023.08.11")),
                hr(),
                ),
      sidebarPanel(align = "center", width = 7, style="padding:2% 15%",
               #h4("What does it do?") %>% u(),
               p("This script use ",u("historical data"), "from different investments, to simulate the purchase of a ", u("fixed value"), "of the ", u("defined asset, every 30 days"), "starting from the", u("start date"), "until today(*)."),
               br(),
               p(text_col(color="orange", 
                         em("*this strategy is called DCA. Scroll to the bottom to find a description and some references")),
               ),                  
         ),
    ),

  # NAVBAR ####
  navbarPage(theme = shinytheme("superhero"),
    title = "Select one Investment Strategy:",
    
    # DCA SIMULATOR ####
    tabPanel(title=strong("DCA simulator"),
             
     #> PARAMETER SETTING ####
     ## fluidRow() layout, desined to occupy 2 rows on computer (2 x 12 width = 24)
     ## Use gray backgroun for input, and black for results. 
     fluidRow(
       
       #>> LEFT SECTION - Instructions ####
       column(width = 3, align="center",
          h4("How to use this tool?") %>% u(),
          p(align="left", style="margin-left:10px",
            sstrong("1. Choose one asset"), br(),
            sstrong("2. Set Amount to invest")," and", sstrong("inflation correction"), br(),
            sstrong("3. Select a Date in the past"),".",br(),
            
          ),
          p("all paramenters can be ", text_col("changed dynamically", col="#84b0fa"),
            "to test their influence on the final outcome."),
          br(),
          p(em("How much would you have earned if you did so?")),
          p(em("Was there a best time to begin investing?")),
          
          hr(),
          div( # separate box
            
            #div(style="background:white; color:black; font-size:75%; width:70%",
            div(align="left", style="font-size:75%; width:100%; color:#000000; background-color: #FFFFFFb0; padding:20px",
                h4(u("Common investments Symbols:"), align="center", style="margin-top:0px"),
                p(style = "margin-left:10px", em(
                  strong("^GSPC"), "=  S&P500 (Index)",br(),
                  strong("^IXIC"), "=  NASDAQ (Index)",br(),
                  strong("^DJI"), "=  Dow Jones Industrial Average (Index)",br(),
                  strong("^TNX"), "=  Treasury Yield 10 years (Index)",br(),
                  strong("^ERIX"), "=  European Revewable Energy Total (Index)",br(),
                  br(),
                  strong("SPY"), "=  S&P500 Index Tracker (ETF)",br(),
                  strong("VTI"), "=  Vangard Total Stock (ETF)",br(),
                  strong("EEM"), "=  Emerging Markets (ETF)",br(),
                  strong("EWI"), "=  Italian Market (ETF)",br(),
                  br(),
                  strong("GOOG"), "=  Google (Stock)",br(),
                  strong("AAPL"), "=  Apple (Stock)",br(),
                  br(),
                  strong("BTC-USD"), "=  Bitcoin (Crypto)",br(),
                  strong("ETH-USD"), "=  Ethereum (Crypto)",br(),
                )),
            ),
          ),
       ),

       #>> INPUT: middle section ####
      column(width=6, align="center",
          fluidRow(
             column(7, align="center", style="padding-left:0px;padding-right:0px",
               sidebarPanel(width=12, # INPUT BOX
                  h4("1. Chose on Asset from", style="padding-top:0px",
                     a("Yahoo Finance", href="https://finance.yahoo.com/lookup/?guccounter=1"),),
                  
                  h5(align="left", 
                    "a. Browse Yahoo Finance Search Engine",
                    a("(link)", href="https://finance.yahoo.com/lookup/?guccounter=1"),
                    "to find the correct", text_col("Symbol spelling"), "for the desired investment.", ),
                  h5(align="left", 
                     "b. Paste it below and hit", code_col('Search')),
                  
                  # split columns in 2 parts (to align search box to search button)
                  fluidRow(
                    column(7, 
                           textInput("symbol", label="", value = "SPY",  width = "100%",
                                                  placeholder = "Symbol Name"),),
                    column(5, align="left",
                           actionButton("symbolsubmit", label = "Search", width = "100%"),
                    ),
                  ),
                  p(style="font-size:90%", em("Note: some symbol names may include special char.",br(),
                  "(e.g. include ",code_col("^", text_col = "black"), "for", code_col("^GSPC", text_col = "black")),")"),
                  ),
             ),
             column(5, style="padding-left:0px;padding-right:0px",
                sidebarPanel(width=12, # INPUT BOX
                             h4("2a. Investment Amount"),
                             # split columns in 2 parts (to align search box to search button)
                             fluidRow(style="margin-bottom:-20px",
                               column(2, style="padding:0px",
                                      align="right", h5("Buy")),
                               column(5, numericInput("monthly_inv", width="100%", label=NULL,
                                          value = 100, min = 0, step = 100)),
                               column(5, style="padding:0px",
                                      align="left", h5("$ every month")),
                             ),
                             
                             hr(),
                             h4("2b. Correct for inflation"),
                             div(style="width:90%;font-size:80%;margin:0px",
                                 em("Purchasing power of $ was higher in the past then it is now.",)),
                             checkboxInput("infl_correction", value = FALSE,
                                           label = span(style="color:#84b0fa;font-size:125%",
                                                        "Tick to correct for inflation.")),
                ),
             ),
          column(12, style="padding-left:0px;padding-right:0px",
             sidebarPanel(width=12, # INPUT BOX
                          style="padding:0px 10%",
                          h4("3. Select investment Start Date", style="padding-top:15px"),
                          plotOutput("market", height = "200px"),
                          div(style="padding-left:9%;padding-right:7%",
                            uiOutput("ui_startDate"),
                            ),
                          p("Investment duration:", strong(textOutput("duration", inline = T))),
             ),
             ),
          ),
          ),
          #>> RIGHT SECTION - Simulation ####
          column(width=3, align="center", style="background:#FFFFFF;color:black",
                    
                 h3("Simulation Results"),
                 fluidRow(style="margin-bottom:-20px",
                   column(3, align="right", style="padding-right:0",
                    h4(strong("Selected Strategy:")),
                   ),
                   column(9, style="padding-right:25px",
                    htmlOutput("strategy"),
                   ),
                   ),
                  hr(),
                 fluidRow(align="center",style="margin-bottom:-20px",
                   column(8,
                          div(style="background:#f7972055",
                              tableOutput("endopoints"),
                              ),
                   ),
                   column(4,style="padding:10px 5px;padding-right:20px",
                          h4(strong("Average Yearly Returns: "),
                          div(style="color:black;background:#84b0fa;width:75%;padding:15px",
                            strong(textOutput("apy", inline = T),"%"),
                            ),
                          ),
                   ),
                   ),
                  hr(),
                  plotOutput("end_plot", height = "325px", width = "100%"),
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
  fluidRow(align="center",
           column(4,
            sidebarPanel(width=12,
                  h3("Investment breakdown, year by year"),
                  p("some descriptive text")
                  ),
           ),
           column(8,
                  h3("Comparison: invested vs. value"),
                  plotOutput("yearly", height = "300px"),
           ),
           column(2,
                  "")
  ),
  hr(),
  
  sidebarLayout(
    sidebarPanel(style="margin:100px -70px 0px 70px; background-color:#FFFFF99",
      h3("What is Dollar Cost Averaging?"), 
    ),
    mainPanel(width=7, style="margin:50px 0; padding:5rem;padding-left:10rem;background-color:#FFFFFF45",
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
