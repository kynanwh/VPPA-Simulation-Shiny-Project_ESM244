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
library(tseries)
library(forecast)
library(profvis)
library(leaflet)
library(sf)
library(plyr)

### Data ###

#CAISO Wholesale Price Data
caiso_price <- read_csv("caiso_2018_hourly_byhub.csv")
caiso_single <- read_csv("caiso_2018_hourly_avghubs.csv")
caiso_day <- read_csv("caiso_2015-2018_daily.csv")

#Renewable production data 
renew_gen <- read_csv("renew_prod_final.csv")

#Location Data for Renewable Projects 
ca_projs <- read_csv("renewable_metadata.csv")


### Define UI for application that draws a histogram ###

ui <- fluidPage(
  theme = shinytheme("cosmo"),
   
   # Application title
   titlePanel("Revenue Model of Renewable Energy Virtual Power Purchase Agreement"),
  
  navbarPage("Welcome!",
             
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
             
             tabPanel("Project Options",
                      
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("renewtype", label = h4("Renewable Energy Type"), 
                                             choices = list("Solar" = "Solar", 
                                                            "Wind" = "Wind"),
                                             selected = "Solar"),
                          
                          hr(),
                          fluidRow(column(3, verbatimTextOutput("value")))
                          
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          leafletOutput(outputId = "map")
                        )
             )
             ),
             
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
                                      choices = list("Whitewater (solar)" = "Whitewater",
                                                     "Sonoma (solar)" = "Sonoma",
                                                     "Lompoc (solar)" = "Lompoc",
                                                     "Fairfield (wind)" = "Fairfield",
                                                     "Rosamond (wind)" = "Rosamond"),
                                      selected = 1)
                        ),
                        mainPanel(
                          plotOutput("revgen"),
                          plotOutput("cashflow"))
                      )
               
               
             )
             )
  )
   
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$wholeprice <- renderPlot({
     
     #put caiso wholesale price in ts format for forecasting
      
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
      
      ## Visualize Data ##
      
      #use reactive function to make data column "Date" same length as "input:date" - lenght = 2
      new_caiso_day <- reactive({
        dplyr::filter(caiso_day_complete, between(Date ,input$date[1], input$date[2]))
      })
     
      #Format axis
      breaks_qtr = seq(from = min(caiso_day_complete$Date), 
                       to = max(caiso_day_complete$Date), by = "2 month")
      
      labels_year = format(seq(from = min(caiso_day_complete$Date), 
                               to = max(caiso_day_complete$Date), by = "1 year"), "%Y")
      
      labs = c(sapply(labels_year, function(x) {
        c(x, rep("", 5))
      }))
      
      range <-  c(as.Date('2015-01-01'), as.Date('2020-01-01'))
      
      #ggplot
     ggplot(new_caiso_day(), aes(x= Date, y = price)) +
       geom_line() +
       geom_ribbon(aes(ymin = lower, ymax= upper),
                   alpha = 0.1,
                   fill = "coral") +
       ylab("Price ($/Megawatt-Hour)") +
       xlab("") +
       labs(title = "Historical and Forecasted Daily Wholesale Electricity Prices in California (2015 - 2020)") +
       theme_set(theme_bw()) +
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.3, 
                                       size = 18,
                                       margin = margin(0,0,20,0)),
             axis.text = element_text(size = 12),
             axis.title.x = element_text(size = 14,
                                         margin = margin(10,0,0,0)),
             axis.title.y = element_text(size = 14,
                                         margin = margin(0,10,0,0))) +
       scale_x_date(labels = labs, 
                    breaks = breaks_qtr, 
                    #limits = as.Date(c('2015-01-01','2020-01-01')), 
                    name = "Year",
                    expand = c(0,0))
     
   })
   
   output$map <- renderLeaflet({
     
     #Setting geometry
     projs_sf <- st_as_sf(ca_projs, coords = c("longitude", "latitude"), crs = 4326)
     
     st_crs(projs_sf) = 4326
     
     #matching png files to coordinate points
     myicons <- iconList(
       Fairfield = makeIcon("wind.png",iconWidth = 30, iconHeight = 30),
       Rosamond = makeIcon("wind.png", iconWidth = 30, iconHeight = 30),
       Whitewater = makeIcon("sunny.png",iconWidth = 38, iconHeight = 38),
       Sonoma = makeIcon("sunny.png", iconWidth = 38, iconHeight = 38),
       Lompoc = makeIcon("sunny.png", iconWidth = 38, iconHeight = 38)
     )
     
     # Filter based on input selection from checkbox input
     projs <- projs_sf %>% 
       filter(Type %in% input$renewtype) 
     
     # Creating map
     leaflet(projs_sf) %>% 
       addTiles() %>% 
       addMarkers(data = projs, 
                  icon = ~myicons[name], 
                  popup = paste( "<b>Name:</b>", projs_sf$name, '<br/>',
                                 "<b>Project Type:</b>", projs_sf$Type, '<br/>',
                                 "<b>Generation:</b>", projs_sf$total_output_mwh, "MWh", '<br/>',
                                 "<b>Size:</b>", projs_sf$size_mw, "MW"  )) %>% 
       setView(lng=-119.535242, lat= 36.547102, zoom = 5)
     
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
     
     #Set colors
     #colors <- ifelse(
       
    #   cash_output > 0,
    #   "green",
    #   "red"
    # )
     
     
     ggplot(cash_df, aes(date_string, cash_output)) +
       geom_line(size = 0.7,
                 color = "dimgray") +
       scale_color_manual(values = c("green")) +
       theme_classic() +
       ylab("Revenue (USD)") +
       xlab("") +
       labs(title = "Revenue Generated from PPA Agreement") +
       theme_set(theme_bw()) +
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.5, 
                                        size = 18,
                                       margin = margin(50,0,20,0)),
              axis.text = element_text(size = 12),
             axis.text.x = element_text(size = 14,
                                        angle = 60,
                                        hjust = 1,
                                        margin = margin(5,0,0,0)),
             axis.title.y = element_text(size = 14,
                                         margin = margin(0,10,0,0))) +
       scale_x_datetime(date_breaks = "1 month", 
                        date_labels = "%B",
                        expand = c(0,0))
  
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
     
     
     #Create values for geom_ribbon 
     
     positive_rev <- ifelse(gen_day1$production > 0, 
                            ifelse(wholesale_day1$price > input$ppa),
                            wholesale_day1$price - input$ppp,
                            0, 0)
      
     
     
     
     
     #Create normalizer for second y-axis (production (MWh))
     normalizer <- 1.5
     
     ggplot(wholesale_day1) +
       geom_line(aes(x = datetime, 
                     y = price, 
                     color = "line1"), 
                 size = 1,
                 show.legend = TRUE) +
       geom_line(data = gen_day1, aes(x = datetime, 
                                      y = production*normalizer,
                                      color = "line2"),
                 size = 1,
                 show.legend = TRUE) +
       geom_hline(aes(yintercept = input$ppa,
                      color = "line3"),
                  size = 1,
                  show.legend = TRUE) +
       scale_colour_manual(name = "Key:",
                          values = c("line1" = "deeppink3",
                                     "line2" = "goldenrod",
                                     "line3" = "dodgerblue3"),
                          labels = c("Wholesale Price",
                                     "Energy Production",
                                     "PPA Price")) +
       scale_y_continuous("Price (USD)", 
                          sec.axis = sec_axis(~./normalizer, 
                                              name = "Production (MWh)",
                                              breaks = c(0,15,30,45,60)),
                          breaks = c(0,20,40,60,80),
                          limits = c(0,80)) +
       scale_x_datetime(date_breaks = "1 hour", 
                        date_labels = "%H",
                        expand = c(0,0)) +
       labs(title = "How Revenue from a vPPA is Generated (An Example Over One Day)",
            x = "Hour") +
       theme_set(theme_bw()) +
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.5, 
                                       size = 18,
                                       margin = margin(0,0,20,0)),
             axis.text = element_text(size = 16),
             axis.title.x = element_text(size = 14,
                                        margin = margin(10,0,0,0)),
             axis.title.y = element_text(size = 14,
                                        margin = margin(0,10,0,10)),
             axis.title.y.right = element_text(size = 14,
                                               margin = margin(0,10,0,10)),
             legend.text = element_text(size = 12),
             legend.title = element_text(size = 14),
             legend.position = "bottom",
             legend.direction = "horizontal")
     
     })

}

# Run the application 
shinyApp(ui = ui, server = server)

