
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(datasets)
library(networkD3)
data(MisLinks)
data(MisNodes)


shinyServer(function(input, output) {
  # Return the requested dataset
  colInput <- reactive({
    switch(input$column,
           "area" = "area", 
           "peri" = "peri", 
           "shape"= "shape",
           "perm" = "perm" )
  })
  
  output$force <- renderForceNetwork({
        forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
                     Target = "target", Value = "value", NodeID = "name",
                     Group = "group", opacity = input$opacity)
  })
      
  
  
  output$distPlot <- renderPlot({
  
    # generate bins based on input$bins from ui.R
    column = colInput()
    x    <- rock[, column]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })
  
  output$caption <- renderText({
    column = paste("Rock Data : ", colInput(), sep="")
  })

  output$clustPlot <- renderPlot(
    {
      column = colInput()
      x    <- rock[, column]
      d = dist(x)
      plot(hclust(d))
    })
  
  

})
