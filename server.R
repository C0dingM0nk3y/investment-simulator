library(shiny)
library(shinythemes)
library(quantmod)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # BUILD UI using DEFAULT/USER DATA ####
  
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
    df[,"Price_Adj"] <- with(df, Adjusted)
    
    # ADD Posit Column (for plotting purposes)
    df[,"Date"] <- row.names(df) %>% as.POSIXct()
    # ADD asset name
    df[,"asset"] <- symbol
    
    return(df)
  }
  
  # CREATE UI ELEMENTS and set defaults INPUTS
  REACT <- reactiveValues() 
  
  # OUTPUT: UI + OUTPUT Market plot preview
  # this runs on start-up and after every user chance of asset
  
  # First, create the reactive slider UI (needed to get default input$startdate value
  output$ui_startDate <- renderUI({
    # only runs of first iteration. Later refresh require to hit "SUBMIT"
    REACT$data_full <- symbolData(isolate(input$symbol), #prevent automatic refresh while user is typing
                                  startdate = "1950-01-01") #recovers the earliest available data
    REACT$minDate <- row.names(REACT$data_full) %>% 
      as.Date() %>% min()
    
    REACT$duration <- difftime(today(), REACT$minDate, units = "weeks") %>% 
      divide_by(52) %>%
      floor() 
    
    sliderInput(inputId = "startdate", label="Select Start Date",
                min = isolate(REACT$minDate), max=today()-366,
                value= isolate(REACT$minDate))
    })
  
  # Second, render plot
  output$market <- renderPlot({
    data <-  REACT$data_full %>%
      pivot_longer(cols = starts_with("Price_"), names_to = "PriceMethod", values_to = "Price")
    
    # Hide Price_Adj until user ticks the box 
    if (input$infl_correction == FALSE){
      data %<>% subset(!(PriceMethod=="Price_Adj"))}
      
    ggplot(data) +
      geom_line(aes(x=Date, y=Price, color=PriceMethod)) +
      geom_vline(xintercept = as.POSIXct(input$startdate), linetype=2, color="cornflowerblue", linewidth =0.9) +
      ggtitle(head(data$asset,1)) + #name is taken from DF, not from inputs
      scale_color_manual(values = c("Price_AVG" = "black", "Price_Adj" = "brown1")) + 
      annotate(geom="label", 
               label=paste("Start:",input$startdate), 
               hjust=0, fill="cornflowerblue", color="white",
               x=as.POSIXct(input$startdate+180), y=max(data$Price)*0.9) +
      theme_classic(base_size = 12) +
      theme(axis.title.x = element_blank(), legend.title = element_blank(), 
            plot.title=element_text(hjust=0.5, size = 16), #center(50%) and size of title
            #plot.title=element_text(size = 16), #center(40%) and bold title
            plot.margin = margin(c(10,10,10,15)),
            #legend.position= c(0.9, 0.25)) 
            legend.position= "right", legend.margin = margin(0)) 
      })
  
  # UPDATE ASSET ("Search")
  observeEvent(input$symbolsubmit, {
    #update datesets
    REACT$data_full <- symbolData(input$symbol,
                                  startdate = "1950-01-01") #recovers the earliest available data
    REACT$minDate <- row.names(REACT$data_full) %>% 
      as.Date() %>% min()
    
    REACT$duration <- difftime(today(), input$startdate, units = "weeks") %>% 
      divide_by(52) %>%
      floor() 
    
    updateSliderInput(inputId = "startdate", min=REACT$minDate)
  })
  
  # UPDATE duration
  observeEvent(input$startdate,
               REACT$duration <- difftime(today(), input$startdate, units = "weeks") %>% 
                 divide_by(52) %>%
                 floor() 
  )
  
  # OUTPUT: TEXT (Investment Duration)
  output$duration <- renderText(
    REACT$duration %>% paste("years")
  )
  
  output$settings <- renderTable(
    rownames = TRUE, colnames = FALSE,
    {
      #from inputs data
      set_df <- data.frame(Value = input$symbol, row.names = "Investment Name")
      set_df["Start Date", 1] <- input$startdate %>% as.Date() %>% as.character()
      set_df["Total Duration", 1] <- REACT$duration %>% paste("years")
      set_df["Monthly Investment", 1] <- paste0(input$monthly_inv, "$")
      set_df["Inflation Correction", 1] <- input$infl_correction
      set_df
    })
  
  # SIMULATION and ANALYSIS ####
  
  DCA_simulate <- function(symbol, startdate, inv_qnt){
    
    # RE-IMPORT asset data, starting from start date #NOTE FOR FUTURE: this may be a filter set on REACT$data_full
    df <- symbolData(symbol, startdate)
    
    # CREATE SUBSET: 1x BUY EVERY 30 days
    
    #> Starting from startdate, subset table to only show prices on the days where a new investment was done.
    #> NB: market is closed on SAT-SUN. Therefore the market data are missing all the rows corresponding to holidays.
    #> For this reason, 1 investment every 30 days is converted into 1 investment every 20 work-days.
    #> Table is filtered so to only keep one row every 20, starting from startdate
    
    df[,"wDaysFromStart"] <- 1:nrow(df)-1 #workdays count
    df[,"filter"] <- ifelse(df$wDaysFromStart %% 20 ==0, TRUE, FALSE)
    
    ss <- subset(df, filter == TRUE,
                 select = c("Date","Price_AVG", "Price_Adj", "wDaysFromStart"))
    
    # PIVOT for PRICE METHOD
    ss %<>% pivot_longer(cols = starts_with("Price_"), names_to = "PriceMethod", values_to = "Price")
      
    # SIMULATE PURCHASE
    ss[,"buy_value"] <- inv_qnt
    ss[,"buy_qnt"] <- with(ss, buy_value/Price) #Price_AVG/Price_Adj
    ss[,"asset"] <- symbol
    
    # ORGANIZE RESULTS
    res <- ss
    
    return(res)
  }
  
  # summarize DCA simulated data BY YEAR
  DCA_summary <- function(df, infl_correction = FALSE){
    df[,"Year"] <- year(df$Date) #variable for summary
    
    # filter for Price_AVG or Price_Adj
    if (infl_correction == TRUE){
      df %<>% subset(PriceMethod == "Price_Adj")}
    else{
      df %<>% subset(PriceMethod == "Price_AVG")}
    
    sum_df <- df %>% 
      group_by(Year) %>%
      summarise(buy_value = sum(buy_value),
                buy_qnt  = sum(buy_qnt),
                Price = mean(Price),
                )
    
    # cumulative calc
    sum_df[,"cum_Invested"] <- cumsum(sum_df$buy_value)
    sum_df[,"tot_Owned"] <- cumsum(sum_df$buy_qnt)
    
    sum_df[,"cum_Value"] <- with(sum_df, tot_Owned*Price)
    
    # ROI
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
                   
                   REACT$summary <- DCA_summary(REACT$simul, input$infl_correction)
                   
                   # used by different plots
                   duration <- difftime(REACT$simul$Date %>% tail(1), 
                                        REACT$simul$Date %>% head(1),
                                        units = "weeks") %>% 
                     divide_by(52) %>%
                     floor() %>% as.numeric()
                   
                   
                   # additional df required by some plots
                   tidy_df <- pivot_longer(REACT$summary, 
                                           cols=starts_with("cum_"), values_to = "Value",
                                           names_to = "CumulData", names_prefix = "cum_") 
                   
                   df_start <- REACT$simul %>% head(1) #Start data
                   df_last <- REACT$summary %>% tail(1) #Summary end data
                   
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
                   

                   output$end_plot <- renderPlot(
                     {
                       #from df_last
                       end_df <- data.frame()
                       end_df["Total Invested ($)", 1] <- df_last[1, "cum_Invested", drop=T]
                       end_df["Total Value ($)", 1] <- df_last[1, "cum_Value"]
                       end_df["Total Return ($)", 1] <-df_last[1, "PNL"]
                       end_df["Total Return (%)", 1] <-df_last[1, "ROI%"]*100
                       end_df["Yearly Return (%)", 1] <- 100*df_last[1, "ROI%", drop=T]/duration
                       
                       output$endopoints <- renderTable( #render table
                         rownames = TRUE, colnames = FALSE,
                         end_df) 
                       
                       end_plot <- data.frame(
                         Total = factor(c("Invested", "Value", "Returns"), 
                                        levels = c("Invested", "Value", "Returns")), #to ensure correct order in plot legend
                         Value = end_df[1:3, 1]
                       ) 
                       
                       ggplot(end_plot) +
                         geom_col(aes(x=Total, y=Value, fill=Total)) +
                         #scale_fill_manual(values = c("Invested" = "black", "Value" = "blue", "Returns" = "green")) + 
                         theme_light()
                   })
                   
                   output$table <- renderDataTable(
                     REACT$summary
                   )
                   
                 }
  )
  

}