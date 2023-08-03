library(shiny)
library(quantmod)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

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
      geom_vline(xintercept = as.POSIXct(input$startdate), linetype=2, color="lightblue3", linewidth =0.9) +
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
  
  
  DCA_simulate <- function(symbol, startdate, inv_qnt){
    
    # RE-IMPORT asset data, starting from start date #FUTURE: this may be a filter set on REACT$data_full
    df <- symbolData(symbol, startdate)
    
    # CREATE SUBSET: 1x BUY EVERY 30 days
    
    #> Starting from startdate, subset table to only show prices on the days where a new investment was done.
    #> NB: market is closed on SAT-SUN. Therefore the market data are missing all the rows corresponding to holidays.
    #> For this reason, 1 investment every 30 days is converted into 1 investment every 20 work-days.
    #> Table is filtered so to only keep one row every 20, starting from startdate
    
    df[,"wDaysFromStart"] <- 1:nrow(df)-1 #workdays count
    df[,"filter"] <- ifelse(df$wDaysFromStart %% 20 ==0, TRUE, FALSE)
    
    ss <- subset(df, filter == TRUE, select = c("Date","Price_AVG", "Adjusted", "wDaysFromStart"))
    
    # SIMULATE PURCHASE
    ss[,"buy_value"] <- inv_qnt
    ss[,"buy_qnt"] <- with(ss, buy_value/Price_AVG)
    ss[,"asset"] <- symbol
    
    # ORGANIZE RESULTS
    res <- ss
    
    return(res)
  }
  
  # summarize DCA simulated data BY YEAR
  DCA_summary <- function(df){
    df[,"Year"] <- year(df$Date)
    
    sum_df <- df %>% 
      group_by(Year) %>%
      summarise(buy_value = sum(buy_value),
                buy_qnt  = sum(buy_qnt),
                Price_AVG = mean(Price_AVG),
                Adjusted = mean(Adjusted),
                )
    
    # cumulative calc
    sum_df[,"cum_Invested"] <- cumsum(sum_df$buy_value)
    sum_df[,"tot_Owned"] <- cumsum(sum_df$buy_qnt)
    
    # Value of investment (according to price back then)
    #latestPrice <- tail(sum_df$Price_AVG, 1)
    #sum_df[,"cum_Value"] <- sum_df$tot_Owned*latestPrice
    sum_df[,"cum_Value"] <- with(sum_df, tot_Owned*Price_AVG)
    
    # ROI
    #sum_df[,"ratioTEMP"] <- with(sum_df, cum_Value/cum_Invested)
    sum_df[,"PNL"] <- with(sum_df, cum_Value-cum_Invested)
    sum_df[,"ROI%"] <- with(sum_df, PNL/cum_Invested)
    
    #ADJCALC - NOT IMPLEMENTED
    #sum_df[,"buy_adj"] <- with(sum_df, buy_value/Adjusted)
    #sum_df[,"cum_adj"] <- cumsum(sum_df$buy_adj)
    #sum_df[,"cum_adjVal"] <- sum_df$cum_adj*latestPrice
    #sum_df[,"ROI_adj"] <- with(sum_df, (cum_adjVal-cum_Invested)/cum_Invested)
    
    # ORGANIZE RESULTS
    res <- sum_df
    
    return(res)
  }
  
  # Update plots and tables             
  observeEvent(input$runAnalysis,
                 {
                   #isolate variable to avoid constant refreshing while user set parameters
                   REACT$simul <- DCA_simulate(isolate(input$symbol), 
                                                   isolate(input$startdate), 
                                                   isolate(input$monthly_inv))
                   
                   REACT$summary <- DCA_summary(REACT$simul)
                   
                   # used by different plots
                   duration <- difftime(REACT$simul$Date %>% tail(1), 
                                        REACT$simul$Date %>% head(1),
                                        units = "weeks") %>% 
                     divide_by(52) %>%
                     floor() %>% as.numeric()
                   
                   
                   #tidy-up dataframe for ggplot compatibility
                   tidy_df <- pivot_longer(REACT$summary, 
                                           cols=starts_with("cum_"), values_to = "Value",
                                           names_to = "CumulData", names_prefix = "cum_") 
                   
                   output$plot <- renderPlot(
                     tidy_df %>% 
                       ggplot() +
                       geom_line(aes(x=Year, y=Value, color=CumulData)) +
                       theme_light()
                   )
                   
                   output$hist <- renderPlot(
                     tidy_df %>% 
                       ggplot() +
                       geom_col(aes(x=Year, y=Value, fill=CumulData), position=position_dodge()) +
                       theme_light()
                   )
                   
                   output$pnl <- renderPlot({
                     pnl <- REACT$summary
                     pnl[,"is.profit"] <- ifelse(pnl$PNL >= 0, "Profit", "Loss")
                     
                     pnl %>% 
                       ggplot() +
                       geom_col(aes(x=Year, y=PNL, fill=is.profit), position=position_dodge()) +
                       scale_fill_manual(values = list("Profit" = "springgreen3", "Loss" = "brown1"), ) + 
                       theme_light()
                   })
                   
                   output$asset <- renderPlot({
                     asset <- REACT$summary
                     
                     asset %>% 
                       ggplot() +
                       geom_col(aes(x=Year, y=buy_qnt), position=position_dodge()) +
                       theme_light()
                   })
                   

                   output$settings <- renderTable(
                     rownames = TRUE, colnames = FALSE,
                     {
                       #from df_start
                       df_start <- REACT$simul %>% head(1) #Start data
                       
                       end_df <- data.frame(Value = df_start[1,"asset"], row.names = "Investment Name")
                       #end_df <- data.frame(Value = "SPY", row.names = "Investment Name")
                       
                       end_df["Start Date", 1] <- as.character(df_start[1, "Date"]) 
                       end_df["Total Duration", 1] <- duration %>% paste("Years")
                       end_df["Monthly Investment", 1] <- paste0(start_df[1, "buy_value"], "$")
                       end_df
                   })
                  
                   output$endopoints <- renderTable(
                     rownames = TRUE, colnames = FALSE,
                     {
                       #from df_last
                       df_last <- REACT$summary %>% tail(1) #Summary end data
                       
                       end_df <- data.frame()
                       end_df["Total Invested ($)", 1] <- df_last[1, "cum_Invested", drop=T]
                       end_df["Total Value ($)", 1] <- df_last[1, "cum_Value"]
                       end_df["Total Return ($)", 1] <-df_last[1, "PNL"]
                       end_df["Total Return (%)", 1] <-df_last[1, "ROI%"]*100
                       end_df["Yearly Return (%)", 1] <- 100*df_last[1, "ROI%", drop=T]/duration
                       end_df
                   })
                   
                   output$table <- renderDataTable(
                     REACT$summary
                   )
                   
                 }
  )
  

}