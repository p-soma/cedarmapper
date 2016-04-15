
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source("R/cedarFunctions.R")

graphPrep <- function(nodes){
  adjmatrix = cedar.adj(nodes)
  # create an edge list
  return(cedar.graph(adjmatrix))
}

library(cedar)
library(htmlwidgets)
library(cedargraph)
library(shiny)
library(datasets)

data("cedarcircle")
npoints = 100

# d is for data
d = circle.data(r=1,n=npoints, randomize=FALSE)

# lense partitions
d.partitions= cedar.partition(d, l = 4)

# list of clusters using euclidean distance, single linkage, and  gap clustering detection, 
d.clusters  = cedar.clusters(d, d.partitions)

# from clusters create nodes of sets of d
d.nodes     = cedar.nodes(d,d.clusters)

# look for links and build adjacency_matrix
d.adjmatrix = cedar.adj(d.nodes)

# create an edge list
d.graph     = cedar.graph(d.adjmatrix) 


shinyServer(function(input, output) {
  # Return the requested dataset
  colInput <- reactive({
    switch(input$column,
           "X" = "X", 
           "Y" = "Y"
      )
  })
  
  output$graph <- renderSimpleGraph({
    simpleGraph(circle.links, circle.nodes)
  })
    
  
  
  output$distPlot <- renderPlot({
  
    # generate bins based on input$bins from ui.R
    column = colInput()
    
    x    <- as.vector(d.nodes[[1]][[column]])
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })
  
  output$caption <- renderText({
    column = paste("Data Variable : ", colInput(), sep="")
  })


 

})
