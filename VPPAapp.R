#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinythemes)

#read in files
caiso_price <- read_csv("caiso_hourly.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  f
  
  # Application title
  titlePanel("CBS VPPA Revenue Model"),
  
  navbarPage("Heres the main title!",
             
             tabPanel("Map of Projects",
                      
                      # Sidebar
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("type",
                                       "Renewable Type Preference:",
                                       choices = c("Solar",
                                                   "Wind",
                                                   "Solar & Wind"))
                        )
                      ),
                      
            tabPanel("Project Revenue",
                      
                      # Sidebar
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("hub",
                                       "Choose your hub",
                                       choices = c("TH_NP15",
                                                   "TH_SP15",
                                                   "TH_ZP26")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("vPPAPlot")
                      )
                      ))
             
  ))))
             

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$vPPAPlot <- renderPlot({
    
    #Create ggplot of wholesale prices in California by hub
    ggplot(filter(caiso_price, ALIGN == input$hub), aes(x = datetime, y = price)) +
      geom_line()
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

