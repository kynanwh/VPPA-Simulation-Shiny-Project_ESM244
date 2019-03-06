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

#CAISO Wholesale Price Data
caiso_price <- read_csv("caiso_hourly.csv")
caiso_single <- read_csv("caiso_single.csv")
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
                          sliderInput("ppa0",
                                      "Adjust PPA Price",
                                      min = 19,
                                      max = 35,
                                      value = 24),
                          selectInput("renew0",
                                      "Select Renewable Project", 
                                      choices = list("solar1" = "solar1",
                                                     "solar2" = "solar2",
                                                     "solar3" = "solar3"),
                                      selected = 1)
                                     ), 
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("vPPAPlot"),
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
   
   output$vPPAPlot <- renderPlot({
     

     #Create ggplot of wholesale prices in California by hub
     ggplot(caiso_single, aes(x = datetime, y = price)) +
       geom_line() +
       geom_hline(yintercept = input$ppa, size = 1, color = "red")
     
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

