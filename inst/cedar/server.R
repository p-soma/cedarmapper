# CEDAR application, using R Shiny
# server.R  
# data and processing

#' @import htmlwidgets
#' @import cedargraph
#' @import shiny
#' @import plyr
#' @import ggplot2
#' @import shinyjs

library(htmlwidgets)
library(cedargraph)
library(cedar)
library(shiny)
library(plyr)
library(shinyjs)

# see global.R for starting values for each new session 

gm <- graphmapper(x=d, lensefun=simple_lense, partition_count=4, overlap=0.5, partition_method="single", index_method="gap", lenseparam="rw")

####### server
shinyServer(function(input, output, session) {

  selectedDataSet <- observe({
    if(is.null(input$dataSelection)){
      d <<- datasets[[1]] }
    else {
      d <<- datasets[[input$dataSelection]] }
    
    updateSelectInput(session, inputId = "selectedVar",choices = names(d))
    updateSelectInput(session, inputId = "filterVar",  choices = names(d))
    return(d)
  })
  
  output$dataname = renderText(input$dataSelection)
  output$datarows = renderText({nrow(d)})
  output$dataset = renderDataTable({d})
  output$partitionCount = reactive({gm$partition_count})
  
  
  observeEvent(input$runMapper, {
    updateTabItems(session, "tabs", selected = "graph")
    progress <- shiny::Progress$new()
    progress$set(message = "Calculating Clustering", value = 0)
    on.exit(progress$close())
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5 }
      progress$set(value = value, detail = detail)
    }
    
    
    lense_fun = simple_lense
    if(! is.null(input$lenseFunctionSelection)) {
      f = get(input$lenseFunctionSelection) 
      if (is.function(f)){ lense_fun = f}
    }
    
    gm<<- makegraphmapper(x = d, 
                          lensefun = lense_fun, 
                          partition_count=as.numeric(input$partitionCountSelection),
                          overlap = as.numeric(input$overlapSelection)/100.0, 
                          partition_method="single", 
                          index_method="gap",
                          lenseparam = input$filterVar,
                          progressUpdater = updateProgress)
    
    # updateTabsetPanel(session, "tabs", selected = "graph")
    return(gm)
  })
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data) })
  
})
