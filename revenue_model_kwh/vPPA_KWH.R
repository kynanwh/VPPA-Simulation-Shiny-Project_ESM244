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

#CAISO Wholesale Price Data
caiso_price <- read_csv("caiso_2018_hourly_byhub.csv")
caiso_single <- read_csv("caiso_2018_hourly_avghubs.csv")
caiso_day_complete <- read_csv("caiso_2015-2018_daily.csv") %>% 
  dplyr::select(Date, price, lower, upper)
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
                                      choices = list("Whitewater (solar)" = "Whitewater",
                                                     "Sonoma (solar)" = "Sonoma",
                                                     "Lompoc (solar)" = "Lompoc",
                                                     "Fairfield (wind)" = "Fairfield",
                                                     "Rosamond (wind)" = "Rosamond"),
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
      
      ##Set data in as.Date format for reactive component
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
       theme_classic() +
       theme(plot.title = element_text(hjust = 0.5, 
                                        size = 20),
              axis.text = element_text(size = 16),
             axis.text.x = element_text(angle = 60,
                                        hjust = 1)) +
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
     
     #Create normalizer for second y-axis (production (MWh))
     normalizer <- 1.5
     
     ggplot(wholesale_day1) +
       geom_line(aes(x = datetime, y = price), 
                 size = 1,
                 color = "sienna4") +
       geom_line(data = gen_day1, aes(x = datetime, 
                                      y = production*normalizer),
                 size = 1,
                 color = "yellow4") +
       geom_hline(yintercept = input$ppa, 
                  size = 1, 
                  color = "dimgray") +
       scale_y_continuous("Price (USD)", 
                          sec.axis = sec_axis(~./normalizer, 
                                              name = "Producion (MWh)")) +
       scale_x_datetime(date_breaks = "1 hour", 
                        date_labels = "%H",
                        expand = c(0,0)) +
       labs(title = "How Revenue from a PPA is Generated (An Example Over One Day",
            x = "Hour") +
       theme_classic() +
       theme(plot.title = element_text(hjust = 0.5, 
                                       size = 20),
             axis.text = element_text(size = 16))
       
     
     })

}

# Run the application 
shinyApp(ui = ui, server = server)

