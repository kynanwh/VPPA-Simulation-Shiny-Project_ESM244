### Part 1: Wholesale price graph ###

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

#Set data in ts format
caiso_ts_day <- ts(caiso_day$price, frequency = 7, start = c(2015,1))

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


### Part 2: Trend Line ####

output$trend <- renderPlot({
  
  #decompose timeseries and draw out trend
  
  caiso_ts <- ts(caiso_single$price, frequency = 24, start = c(2018,1))
  
  caiso_dc <- decompose(caiso_ts)
  caiso_trend <- caiso_dc$trend
  date_string <- caiso_single$datetime
  caiso_trend2 <- data.frame(date_string, caiso_trend)
  
  caiso_trend2$date_string <- as.Date(caiso_trend2$date_string,
                                      format="%m/%d/%Y")
  
  new_trend_day <- reactive({
    dplyr::filter(caiso_trend2, between(date_string ,input$date[1], input$date[2]))
  })
  
  
  #Create ggplot of wholesale prices in California by hub
  ggplot(caiso_trend2, aes(x = date_string, y = caiso_trend)) +
    geom_line()
  
})