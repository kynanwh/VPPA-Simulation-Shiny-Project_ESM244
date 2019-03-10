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
library(plotly)
library(htmltools)


ca_projs <- read_csv("renewable_metadata.csv")
projs_sf <- st_as_sf(ca_projs, coords = c("longitude", "latitude"), crs = 4326)

st_crs(projs_sf) = 4326

myicons <- iconList(
  Fairfield = makeIcon("wind.png",iconWidth = 30, iconHeight = 30),
  Rosamond = makeIcon("wind.png", iconWidth = 30, iconHeight = 30),
  Whitewater = makeIcon("sunny.png",iconWidth = 38, iconHeight = 38),
  Sonoma = makeIcon("sunny.png", iconWidth = 38, iconHeight = 38),
  Lompoc = makeIcon("sunny.png", iconWidth = 38, iconHeight = 38)
)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("California Renewable Energy Projects"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("renewtype", label = h4("Renewable Energy Type"), 
                         choices = list("Solar" = "Solar", "Wind" = "Wind"),
                         selected = "Solar"),
      
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
  
  output$map <- renderLeaflet({
    
    projs <- projs_sf %>% 
      filter(Type %in% input$renewtype) # Filter based on input selection from checkbox input
    
    # Creating map
    leaflet(projs_sf) %>% 
      addTiles() %>% 
      addMarkers(data = projs, icon = ~myicons[name], popup = paste( "<b>Name:</b>", projs_sf$name, '<br/>',
                                                                     "<b>Project Type:</b>", projs_sf$Type, '<br/>',
                                                                     "<b>Generation:</b>", projs_sf$total_output_mwh, "MWh", '<br/>',
                                                                     "<b>Size:</b>", projs_sf$size_mw, "MW"  )) %>% 
      setView(lng=-119.535242, lat= 36.547102, zoom = 5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



















