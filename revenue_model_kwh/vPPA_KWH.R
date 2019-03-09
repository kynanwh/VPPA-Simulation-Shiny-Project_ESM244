#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(lubridate)
library(forecast)

#CAISO Wholesale Price Data
caiso_price <- read_csv("caiso_2018_hourly_byhub.csv")
caiso_single <- read_csv("caiso_2018_hourly_avghubs.csv")
caiso_4yr <- read_csv("caiso_2015-2018_original.csv")
renew_gen <- read_csv("renew_prod_final.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cosmo"),
   
   # Application title
   titlePanel("CBS VPPA Revenue Model"),
  
  navbarPage("Here's a main title!",
             
             tabPanel("Wholesale Electricity Prices",
                      
                      #sidebar
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("date",
                                      "Select Date Range",
                                      start = "2015-01-01",
                                      end = "2020-12-30",
                                      min = "2015-01-01",
                                      max = "2020-12-31")
                                     ), 
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("wholeprice"),
                          plotOutput("trend"))
                      )),
             
             tabPanel("Project Revenue",
                      
                      #sidebar
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("ppa",
                                      "Adjust PPA Price",
                                      min = 19,
                                      max = 35,
                                      value = 24),
                          selectInput("renew",
                                      "Select Renewable Project", 
                                      choices = list("solar1" = "solar1",
                                                     "solar2" = "solar2",
                                                     "solar3" = "solar3"),
                                      selected = 1)
                        ),
                        mainPanel(
                          plotOutput("cashflow"),
                          plotOutput("revgen"))
                      )
               
               
             )
             )
  )
   
# Define server logic required to draw a histogram
server <- function(input, output) {
   
    output$wholeprice <- renderPlot({
      
      ## Wrangle California 2015-2018 5-minute wholesale price to daily prices ##
      
      #format dates as.POSIXct
      caiso_4yr$date <- as.POSIXct(caiso_4yr$date,
                                   format="%m/%d/%Y %H:%M")
      
      #Remove NA Values
      caiso_na <- which(is.na(caiso_4yr$date), arr.ind=TRUE)
      caiso_nna <- caiso_4yr[-c(caiso_na),]
      
      #transform 5-minute data to daily data taking mean average of 5-minute prices
      caiso_day <- caiso_nna %>% 
        dplyr::group_by(Date = floor_date(date, "1 day")) %>% 
        dplyr::summarise(
          mean(price)
        ) %>% 
        dplyr::rename(price = "mean(price)")
      
      
      ## Forecast wholesale prices 3 years into future ##
      
      #Chose to use autoregressive integrated moving average (ARIMA) instead of Holt-Winters to forecast   becuase data doesn't show strong seasonality or cycles. Holt-Winters test does better with seasonality.  
      
      #get p,d,q values for arima
      caiso_day_pdq <- auto.arima(caiso_day$price) #pdq = [1,1,1]
      
      caiso_day_arima <- arima(caiso_ts_day, order = c(1,1,1))
      
      #forecast
      forecast_caiso_day <- forecast(caiso_day_arima, h = 365) #365 days = 1 year
      plot(forecast_caiso_day)
      
      #pull out mean, upper, and lower values to bind with origional dataset
      caiso_day_forecast_mean <- forecast_caiso_day$mean
      caiso_day_forecast_lower <- forecast_caiso_day$lower
      caiso_day_forecast_upper <- forecast_caiso_day$upper
      
      date_seq <- seq(ymd('2019-01-01'),ymd('2019-12-31'), by = 'days')
      
      #create dataframe with forecast values and data sequence
      caiso_forecast_df <- data.frame(date_seq,
                                      caiso_day_forecast_mean, 
                                      caiso_day_forecast_lower, 
                                      caiso_day_forecast_upper)
      
      caiso_forecast_df_ed <- caiso_forecast_df %>% 
        dplyr::select(date_seq, caiso_day_forecast_mean, X95., X95..1)
      
      colnames(caiso_forecast_df_ed) <- c("Date", "price", "lower", "upper")
      
      #bind with original 2015-2018 wholesale prices
      caiso_day_complete <- rbind.fill(caiso_day, caiso_forecast_df_ed)
      
      caiso_day_complete$Date <- as.Date(caiso_day_complete$Date,
                                         format="%m/%d/%Y")
     
     ## Visualize Data ##
      
      #use reactive function to make data column "Date" same length as "input:date" - lenght = 2
      new_caiso_day <- reactive({
        dplyr::filter(caiso_day_complete, between(Date ,input$date[1], input$date[2]))
      })
     
      #Format axis
      breaks_qtr = seq(from = min(caiso_day_complete$Date), 
                       to = max(caiso_day_complete$Date), by = "3 months")
      
      labels_year = format(seq(from = min(caiso_day_complete$Date), 
                               to = max(caiso_day_complete$Date), by = "1 year"), "%Y")
      
      labs = c(sapply(labels_year, function(x) {
        c(x, rep("", 3))
      }))
      
      #ggplot
     ggplot(new_caiso_day(), aes(x= Date, y = price)) +
       geom_line() +
       geom_ribbon(aes(ymin = lower, ymax= upper),
                   alpha = 0.1,
                   fill = "coral") +
       ylab("Price ($/Megawatt-Hour)") +
       xlab("") +
       labs(title = "Historic and Forecasted California Wholesale Prices (2015 - 2020)") +
       theme_classic() +
       theme(plot.title = element_text(hjust = 0.5, 
                                       size = 20),
             axis.text = element_text(size = 12)) +
       scale_x_date(labels = labs, breaks = breaks_qtr, name = "Year")
     
   })
   
   output$trend <- renderPlot({
     
     #decompose timeseries and draw out trend
     
     caiso_ts <- ts(caiso_single$price, frequency = 24, start = c(2018,1))
     
     caiso_dc <- decompose(caiso_ts)
     caiso_trend <- caiso_dc$trend
     date_string <- caiso_single$datetime
     caiso_trend2 <- data.frame(date_string, caiso_trend)
     
     
     #Create ggplot of wholesale prices in California by hub
     ggplot(caiso_trend2, aes(x = date_string, y = caiso_trend)) +
       geom_line()
     
   })
   
  
   
   output$cashflow <- renderPlot({
     
    ##Variables
     
     #Production
     project_select <- renew_gen %>% 
       filter(project_name %in% input$renew)
     
     production <- project_select$production
     
     #Wholesale Price

     wholesale_price <- caiso_single$price
     
     #PPA Price
     ppa_price <- input$ppa
     
     
    ##Set fuction                            
     cashflow <- function(
       
       production,
       wholesale_price,
       ppa_price
       
     ){
       
       revenue <- production * wholesale_price
       cost <- production * ppa_price
       cash_flow <- revenue - cost
       
     }
     
     cash_output <- cashflow(production, wholesale_price, ppa_price)
     date_string <- caiso_price$datetime
     cash_df <- data.frame(date_string, cash_output)
     
     ggplot(cash_df, aes(date_string, cash_output)) +
       geom_line()
     
   })
   
   output$revgen <- renderPlot({
     
    ##isolate single day for wholesale price and project generation
    
     
      #wholesale price
     wholesale_day1 <- caiso_single %>% 
       dplyr::filter(datetime < ymd_hms("2018-01-02 00:00:00"))
     
     #project generation
     renew_date_string <- wholesale_day1$datetime
     
     project_select <- renew_gen %>% 
       dplyr::filter(Month == "1") %>% 
       dplyr::filter(Day == "1") %>% 
       dplyr::filter(project_name %in% input$renew)
     
     gen_day1 <- data.frame(project_select, renew_date_string) %>% 
       dplyr::rename(datetime = renew_date_string)
     
     #Create normalizer for second y-axis (production (MWh))
     normalizer <- 1.5
     
     ggplot(wholesale_day1) +
       geom_line(aes(x = datetime, y = price)) +
       geom_line(data = gen_day1, aes(x = datetime, 
                                      y = production*normalizer)) +
       geom_hline(yintercept = input$ppa, 
                  size = 1, 
                  color = "red") +
       scale_y_continuous("Price (USD)", sec.axis = sec_axis(~./normalizer, name = "Producion (MWh)"))
     
     })

}

# Run the application 
shinyApp(ui = ui, server = server)

