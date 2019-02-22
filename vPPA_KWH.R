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

#import data
caiso_price <- read_csv("caiso_hourly.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cosmo"),
   
   # Application title
   titlePanel("CBS VPPA Revenue Model"),
  
  navbarPage("Here's a main title!",
             
             tabPanel("Project Revenue",
                      
                      #sidebar
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("hub",
                                       "Choose your hub",
                                       choices = c("TH_NP15",
                                                   "TH_SP15",
                                                   "TH_ZP26"))
                                     ), 
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("vPPAPlot"))
                      ))
             )
  )
   
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$vPPAPlot <- renderPlot({
     
     #Create ggplot of wholesale prices in California by hub
     ggplot(filter(caiso_price, hub == input$hub), aes(x = datetime, y = price)) +
       geom_line()
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

