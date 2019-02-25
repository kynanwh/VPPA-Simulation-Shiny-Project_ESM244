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
library(dplyr)
library(shinythemes)
library(ggplot2)
library(leaflet)
library(sf)

#ca_wind3 <- ca_wind2[c(1:3)]
#saveRDS(ca_wind3, "./ca_wind3.rds")

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  titlePanel("California Renewable Energy Projects"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("renewtype", label = h4("Renewable Energy Type"), 
                         choices = list("Solar" = 1, "Wind" = 2),
                         selected = 1),
      
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  filtered <- reactive ({

   ca_wind2 %>% 
      filter(type == input$renewtype)
    
    #filter(type == input$renewtype)
    
    #ca_wind2 %>% 
    #  filter(type == input$renewtype[1], type == input$renewtype[2])
    
  })
  
  
  output$map <- renderLeaflet({ 
    leaflet() %>% 
      addTiles() %>% 
      addMarkers(data = ca_wind2) %>% 
      setView(lng=-119.535242, lat= 36.547102, zoom = 5)
    
    
    # })  
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
