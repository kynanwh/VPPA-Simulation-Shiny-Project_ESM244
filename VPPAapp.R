#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  # Application title
  titlePanel("CBS VPPA Revenue Model"),
  
  navbarPage("Heres the main title!",
             
             tabPanel("Map of Projects",
                      
                      radioButtons("type",
                                   "Renewable Type Preference:",
                                   choices = c("Solar",
                                               "Wind",
                                               "Solar & Wind"))),
             
             tabPanel("Project Revenue",
                      
                      sliderInput("strike",
                                  "PPA Strike Price:",
                                  min = 25,
                                  max = 60,
                                  value = 50),
                      
                      radioButtons("type",
                                   "Renewable Type Preference:",
                                   choices = c("Solar",
                                               "Wind",
                                               "Solar & Wind")))
             
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})


# Run the application 
shinyApp(ui = ui, server = server)

