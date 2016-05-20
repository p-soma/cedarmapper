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

  # observers, selections
  selectedDataSet <- observe({
    if(is.null(input$dataSelection)){
      d <<- datasets[[1]] }
    else {
      d <<- datasets[[input$dataSelection]] }
    
    updateSelectInput(session, inputId = "selectedVar",choices = names(d))
    updateSelectInput(session, inputId = "filterVar",  choices = names(d))
    return(d)
  })
  
  # this sends an array of means for each node,
  # from the gm$d data frame column
  # of the selected variable to Shiny via the session object
  
  # selectedVar ==> color and data exploration variable 
  selectedVar <- reactive({ 
    if(is.null(input$selectedVar)){ v = names(selectedDataSet())[1]}
    else{ v = input$selectedVar}
    return(v)
  })
  
  observe({
    input$selectedVar
    if (selectedVar() %in% names(gm$d)) {
      vals =  nodePrep(gm, selectedVar())$values
      session$sendCustomMessage(type='nodevalues',message = vals)
    }
  })
  
  # input$nodelist is created by the Javascript HTMLWidget on selection events
  # this wraps that input in reactive context to return 0 when no selection made 
  getNodeList <- reactive({
    thisnodelist <- 0
    if (!is.null(input$nodelist)) {
      thisnodelist <- as.numeric(input$nodelist)
    }
    return(thisnodelist)
  })
  
  # when 'group 1 button is clicked, return currently selected nodes
  group1 <- eventReactive(input$grp1set, {
    nl = as.numeric(getNodeList())
    gm$groups[["group1"]] = nl
    print(gm$groups)
    length(unlist(gm$groups[["group1"]]))
  })
  
  # when 'group 2' button is clicked, return currently selected nodes
  group2 <- eventReactive(input$grp2set, {
    nl = as.numeric(getNodeList())
    gm$groups[["group2"]] = nl
    print(gm$groups)
    length(gm$groups[["group2"]])
  })
  
  ########### outputs
  output$dataname   <- renderText(input$dataSelection)
  output$datarows   <- renderText({nrow(d)})
  output$dataset    <- renderDataTable({d})
  output$nodeCount  <- renderText({paste0(length(gm$nodes), " nodes")})
  output$group1Count <- renderText({ group1() })
  output$group2Count <- renderText({ group2() })
  # custom inputs from CedarGraph html widget
  output$nodeListText = renderText({ getNodeList()})
  

  # graph widget, but only if the mapper object has been created via button
  output$cgplot <- renderCedarGraph({
    input$runMapper
    graphdata <-isolate(
      list(graph_nodes = nodePrep(gm,input$selectedVar), graph_links = linkPrep(gm))
    )
    print("rendering graph")
    cedarGraph(graphdata$graph_links, graphdata$graph_nodes,"500","500")
  })
  
  # button to create mapper, which takes a while
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
    
    return(gm)
  })
  
  
})
