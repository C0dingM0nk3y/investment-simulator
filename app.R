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

# 

# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "investment-simulator",
  
  fluidRow(
    column(3, offset = 1,
           h1("somehting catchy"),
           ),
    column(8,
           h4("then maybe some description here?"),
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
      textInput("asset_name",
                p("Chose one asset to invest into", br(), em("default: SPY = 'S&P500 full index'")), 
                value = "SPY", placeholder = "any asset name supported by Yahoo Finance"),
      p(em("For a guide on Stock and Index names, refer to ", a("Yahoo Finance Symbol List", href="https://finance.yahoo.com/lookup/?guccounter=1", )),
      ),
    ),
  ),
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
