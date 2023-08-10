source("_init.R")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # BUILD UI using DEFAULT/USER DATA ####

  symbolData <- function(symbol, startdate){
    #> get data using quantmod::getSymbolData(), then 
    #> perform basic df tidying, like adding Date column and calculating AVG price

    #sanity check
    startdate <- ifelse(is.null(startdate), 
                        "1950-01-01", #prevent crash on server first load
                        startdate)

    symbol %<>% str_trim() #removes whitespaces from symbol name (prevents errors on search)
    
    # DOWNLOAD LATEST DATA FROM YAHOO
    xts <- getSymbols(str_trim(symbol), 
                      from = startdate,
                      auto.assign = FALSE) #required to assign to custom variable name
    
    # CALCULATE AVERAGE PRICE 
    # removes symbol name from df headers #eg. from SPY.Open to SPY
    df <- data.frame(xts) #converts from xts object to standard df
    
    symbolName <- str_replace(symbol, pattern = "-", ".") #replace "-" with "." (dataframe automatic name change)
    colnames(df) %<>% str_remove(symbolName)  %>% #symbol that contain special characters
      str_remove(".")
    df %<>% drop_na() #filters out missing val
    
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
    
    REACT$duration_n <- difftime(today(), REACT$minDate, units = "weeks") %>% 
      divide_by(52) %>%
      floor()  %>% as.numeric()
    
    sliderInput(inputId = "startdate", label=NULL,
                min = REACT$minDate, max=today()-366,
                value= REACT$minDate)
    })
  
  # Second, render plot
  output$market <- renderPlot({
    data <-  REACT$data_full %>%
      pivot_longer(cols = starts_with("Price_"), names_to = "PriceMethod", values_to = "Price")
      
    ggplot(data) +
      geom_line(aes(x=Date, y=Price, color=PriceMethod)) +
      geom_vline(xintercept = as.POSIXct(input$startdate), linetype=2, color="cornflowerblue", linewidth =0.9) +
      ggtitle(head(data$asset,1)) + #name is taken from DF, not from inputs
      scale_color_manual(values = c("Price_AVG" = "black", "Price_Adj" = "orange")) + 
      annotate(geom="label", 
               label=paste("Start:",input$startdate), 
               hjust=0, fill="cornflowerblue", color="white",
               x=as.POSIXct(input$startdate+180), y=max(data$Price)*0.9) +
      theme_classic(base_size = 12) +
      theme(axis.title.x = element_blank(), legend.title = element_blank(), 
            plot.title=element_text(hjust=0.45, size = 16), #center(50%) and size of title
            #plot.title=element_text(size = 16), #center(40%) and bold title
            plot.margin = margin(c(10,15,10,15)),
            #legend.position= c(0.9, 0.25)) 
            legend.position= "top", legend.margin = margin(0)) 
      })
  
  # UPDATE ASSET ("Search")
  observeEvent(input$symbolsubmit, {
    #update datesets
    REACT$data_full <- symbolData(input$symbol,
                                  startdate = "1950-01-01") #recovers the earliest available data
    REACT$minDate <- row.names(REACT$data_full) %>% 
      as.Date() %>% min()
    
    REACT$duration_n <- difftime(today(), input$startdate, units = "weeks") %>% 
      divide_by(52) %>%
      floor() %>% as.numeric()
    
    updateSliderInput(inputId = "startdate", min=REACT$minDate)
  })
  
  # UPDATE on input change
  observeEvent(list(input$startdate, input$infl_correction, input$monthly_inv),
               updateSimulation()
  )
  
  # OUTPUT: TEXT ####
  output$duration <- renderText(
    REACT$duration_n %>% paste("years")
  )
  
  # CAN BE REMOVED - NOT USED ANYMORE
  output$settings <- renderTable(align = "r",
    rownames = TRUE, colnames = FALSE,
    {
      #sanity check
      startdate <- ifelse(is.null(input$startdate), 
                          "1950-01-01", #prevent crash on server first load
                          input$startdate)
      #from inputs data
      set_df <- data.frame(Value = paste0("*",input$symbol,"*"), row.names = "Investment Name")
      set_df["Start Date", 1] <- startdate %>% as.Date() %>% as.character() #currently causing error message
      set_df["Total Duration", 1] <- REACT$duration_n %>% paste("years")
      set_df["Monthly Investment", 1] <- paste0(input$monthly_inv, "$")
      set_df["Inflation Correction", 1] <- input$infl_correction
      set_df
    })
  
  # ENDPOINT: TEXT
  output$strategy <- renderText({
     sprintf("Buy %s worth of %s <u>every 30 days</u>, <br> from %s to %s %s",
             sstrong(paste0(input$monthly_inv, "$")), sstrong(input$symbol), 
             sstrong(input$startdate), today(), sstrong(paste0("(",REACT$duration_n, "years)")))
     }) 
  
  output$apy <- renderText({
    last_df <- REACT$summary %>% tail(1)
    round(last_df[1, "ROI%",drop=T]/REACT$duration_n*100,2)
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
                asset = head(asset, 1)
                )
    
    # cumulative calc
    sum_df[,"cum_Invested"] <- cumsum(sum_df$buy_value)
    sum_df[,"tot_qnt"] <- cumsum(sum_df$buy_qnt)
    
    sum_df[,"cum_Value"] <- with(sum_df, tot_qnt*Price)
    
    # ROI
    sum_df[,"PNL"] <- with(sum_df, cum_Value-cum_Invested)
    sum_df[,"ROI%"] <- with(sum_df, PNL/cum_Invested)
    
    # ORGANIZE RESULTS
    res <- sum_df
    
    return(res)
  }
  
  updateSimulation <- reactive(
    {
      #isolate variable to avoid constant refreshing while user set parameters
      REACT$simul <- DCA_simulate(isolate(input$symbol), #only input$symbol is isolated, so it only changes if user hit "search"
                                  input$startdate, 
                                  input$monthly_inv)
      REACT$summary <- DCA_summary(REACT$simul, input$infl_correction)
      
      # used by different plots
      REACT$duration_n <- difftime(REACT$simul$Date %>% tail(1), 
                           REACT$simul$Date %>% head(1),
                           units = "weeks") %>% 
        divide_by(52) %>%
        floor() %>% as.numeric()
    }
  )
  
  # Update shared dataset at every change of variable             
  observeEvent(input$symbolsubmit,
               {updateSimulation()})
       
  # PLOTS AND TABLES ####
  # > Yearly Accumulation ####
   output$dca_qnt <- renderPlot({
     updateSimulation()
     
     REACT$summary %>% 
       ggplot() +
       geom_col(aes(x=Year, y=tot_qnt, fill="Total"), position=position_identity()) +
       geom_col(aes(x=Year, y=buy_qnt, fill="Each Year"), position=position_identity()) +
       scale_y_log10(minor_breaks = scales::breaks_width(10))+
       #scale_fill_manual(values = c("Total Qnt" = "orange", "Added Qnt" = "#619CFF")) +
       geom_line(aes(x=Year, 
                     y=Price/max(Price)*max(tot_qnt), color="Price")) + #scale to show on same scale
       scale_color_manual(values="red") +
       ylab("Shares Qnt (Log10)") +
       ggtitle("Qnt of purchased asset (each year and total)") +
       theme_light(base_size = 14) +
       theme(legend.position = "right", 
             plot.title=element_text(hjust=0.5, face="bold")) 
   })
  
  # > Yearly DCA ####
     output$dca_val <- renderPlot({
       updateSimulation()
       
       REACT$summary %>% 
         ggplot() +
         geom_col(aes(x=Year, y=cum_Invested, fill="Total"), position=position_identity()) +
         geom_col(aes(x=Year, y=buy_value, fill="Each Year"), position=position_identity()) +
         scale_fill_manual(values = c("Each Year" = "orange", "Total" = "#619CFF")) +
         scale_y_continuous(breaks = scales::breaks_width(5000), 
                            minor_breaks = scales::breaks_width(1000)) +
         ylab("Invested Value (Buy)") +
         ggtitle("Amount of invested money (each year, and total)") +
         theme_light(base_size = 14) +
         theme(legend.position = "right",
               plot.title=element_text(hjust=0.5, face="bold")) 
     })
  
  # OLD: REMOVE? ####
  output$hist <- renderPlot({
    updateSimulation()
    
    tidy_df <- pivot_longer(REACT$summary, 
                            cols=starts_with("cum_"), values_to = "Value",
                            names_to = "CumulData", names_prefix = "cum_") 
    tidy_df %>% 
      ggplot() +
      geom_col(aes(x=Year, y=Value, fill=CumulData), position=position_dodge()) +
      scale_fill_manual(values = c("Invested" = "orange", "Value" = "#619CFF")) +
      scale_y_continuous(breaks = scales::breaks_width(50000), 
                         minor_breaks = scales::breaks_width(10000)) +
      theme_light(base_size = 14) +
      theme(legend.position = "right") 
  })
     
                   
   output$pnl <- renderPlot({
     updateSimulation()
     
     pnl <- REACT$summary
     pnl[,"is.profit"] <- ifelse(pnl$PNL >= 0, "Profit", "Loss")
     
     pnl %>% 
       ggplot() +
       geom_col(aes(x=Year, y=PNL, fill=is.profit), position=position_dodge()) +
       scale_fill_manual(values = list("Profit" = "springgreen3", "Loss" = "brown1"), ) + 
       scale_y_continuous(breaks = scales::breaks_width(50000), 
                          minor_breaks = scales::breaks_width(10000)) +
       ylab("Value") +
       theme_light(base_size = 14) +
       theme(legend.position = "none") 
   })
                   
   output$asset <- renderPlot({
     updateSimulation()
     
     asset <- REACT$summary
     asset_name <- head(asset$asset,1)
     
     asset %>% 
       ggplot() +
       geom_col(aes(x=Year, y=buy_qnt, fill=asset_name), #asset name
                position=position_dodge()) +
       scale_fill_manual(values="gray") +
       scale_color_manual(values="red") +
       geom_line(aes(x=Year, 
                     y=Price/max(Price)*max(buy_qnt), color="Price")) + #scale to show on same scale
       ylab("Share Buy (per year)") +
       theme_light(base_size = 14) +
       theme(legend.position = "top") +
       guides(fill=guide_legend(title="Asset Name:"), color=guide_legend(title="History:"))
   })
                   

output$end_plot <- renderPlot(
                     {
                       updateSimulation()
                       
                       df_start <- REACT$simul %>% head(1) #Start data
                       df_last <- REACT$summary %>% tail(1) #Summary end data
                       duration <- REACT$duration_n
                       
                       #from df_last
                       end_df <- data.frame()
                       end_df["Invested", 1] <- df_last[1, "cum_Invested", drop=T] %>% round(0)
                       end_df["Value", 1] <- df_last[1, "cum_Value"] %>% round(0)
                       end_df["Returns (PNL)", 1] <-df_last[1, "PNL"] %>% round(0)
                       end_df["Returns (%)", 1] <- round(df_last[1, "ROI%"]*100,1)
                       
                       colnames(end_df) <- c("Simulated Data")
                       
                       # ENDPOINT: TABLE
                       output$endopoints <- renderTable(align = "r", #render table
                         rownames = TRUE, colnames = TRUE,
                         {
                           # Text formatting (units)
                           end_df[1:3,1] %<>% format(big.mark=".", decimal.mark = ",") %>%
                             paste("$") %>% str_replace("NA .", "")
                           
                           end_df[4,1] %<>% paste("%") %>% str_replace("NA %", "")
                           
                           end_df
                           }) 
                       
                       # ENDPOINT: PLOT
                       end_plot <- data.frame(
                         Total = factor(c("Invested", "Value", "Returns (PNL)"), 
                                        levels = c("Invested", "Value", "Returns (PNL)")), #to ensure correct order in plot legend
                         Value = end_df[1:3, 1]
                       ) 
                       
                       ggplot(end_plot) +
                         geom_col(aes(x=Total, y=Value, fill=Total), color="black") +
                         ggtitle("Simulated Profits") +
                         scale_y_continuous(breaks = scales::breaks_width(50000), 
                                            minor_breaks = scales::breaks_width(10000)) +
                         scale_fill_manual(values = c("Invested" = "orange", "Value" = "#619CFF", "Returns (PNL)" = "aquamarine")) + 
                         geom_text(aes(x=Total, 
                                       #y=Value + max(Value/20), #calculate max value then /20 = 5% of plot size 
                                       y=Value*0.5, # middle of bar 
                                       label=format(Value, big.mark=".", decimal.mark=",") %>% paste("$")),
                                  size=4
                                   ) +
                         theme_classic(base_size = 16) +
                         theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold"),
                               axis.title.y = element_blank(),
                               plot.title=element_text(hjust=0.5, size = 18), #center(50%) and size of title
                               plot.margin = margin(c(10,10,10,15)),
                               legend.position= "none")
                   })
                   
   output$table <- renderDataTable(
     REACT$summary
   )
                   
}