# example_app.R
# very basic example of using this widget in an RStudio Shiny app

#' @import htmlwidgets
#' @import cedargraph
#' @import shiny
#' @import plyr
#' @import ggplot2

# library(htmlwidgets)
# library(cedargraph)
# library(cedar)
# library(shiny)
# library(plyr)

# TODO: REPLACE THIS WITH DATA PREPARATION
# data("cedarcircle")

# source("cedarFunctions.R")
# source("cedarFunctions.R")

d = circle.data(r=1,n=1000, randomize=TRUE)

d.partitions= cedar.partition(d, l = 4)
d.clusters  = cedar.clusters(d, d.partitions)
d.nodes     = cedar.nodes(d,d.clusters)
graph_nodes = nodePrep(d.nodes)
graph_links = linkPrep(d.nodes)

# nodedata = 
ui <- 
  fluidPage(
    h3("CedarProject: Node Data"),
    fluidRow(
      column(6, 
             wellPanel(
               selectInput("variableselect", label = h3("Select box"), 
                           choices = list("X" = 'X', "Y" = 'Y'), 
                           selected = 1),
            cedarGraphOutput("cedargraph")
              )
      ),
      column(6, wellPanel(
        uiOutput("selectedVariable"),
        p("variance:", uiOutput("variance")),
        uiOutput("nodeListInput"),
        uiOutput("nodeValuesInput"),
        conditionalPanel(
          condition="(input.nl)",
            plotOutput("nodePlot")
        )
      )
      
    )
  )
)





server <- function(input, output, session) {
  
  selectedVar = reactive({ 
    v = input$variableselect
    return(v)})
  
  output$selectedVariable <- renderText({selectedVar()})
                                        
  output$variance <- renderUI({p(var(getValues()))})
  
  observe({
    
    print(cat(as.numeric(input$nodelist)))
    if (!is.null(input$nodelist)) {
      print("class:")
      print(class(getNodeList()))
      print(1 %in% getNodeList())
    }
    
  })
  
  getNodeList <- reactive({
    nl <- NULL
    if (!is.null(input$nodelist)) {
      nl <- as.numeric(input$nodelist)
    }
    return(nl)
    
  })
  
  getNodes <- reactive({
    ns <- getNodeList()
    selected_nodes <- NULL
    if( ! is.null(ns)) {
      selected_nodes <- graph_nodes[graph_nodes$name %in% ns,]}
    n = as.vector(selected_nodes["name"])   #  "name" is 'nodeid' as used in the node prep script
    # print(n)  # debug
    return(n)
  })
  
  getValues <- reactive({
    # TODO: create input$varname, e.g. from dropdown
    node_ids = getNodes()
    if( is.null(node_ids) || nrow(node_ids)==0 ){
      return(0)
    }
   
    n = d.nodes[as.numeric(unlist(node_ids))]
    datarows = ldply(n, data.frame)
    selectedVariable = input$variableselect 
    return(get(selectedVariable,datarows))
  })
  
  output$cedargraph <- renderCedarGraph({
    # TODO: replace with node data here
    cedarGraph(graph_links, graph_nodes,250,250)
  })
  
  output$nodePlot = renderPlot({
    v = getValues()
    qplot(v,
          main = "Histogram of Selected Variable", 
          xlab = "Data Values",
          fill=I("blue"), 
          col=I("black"), 
          alpha=I(.2))
  })
  
  output$nodeListInput <- renderUI({
    textInput("nl","selected nodes", paste(getNodeList(), sep=",", collapse = ","))
  })
  
  output$nodeValuesInput <- renderUI({
    textInput("vl","selected values", paste(getValues(), sep=",", collapse = ","))
  })
  
  output$randomgraph <- renderCedarGraph({
    graph.data= randGraphData(n=input$nodecount)
    links = graph.data$links
    cedarGraph(LinksDF =links, NodesDF = graph.data$nodes,250,250)
    session$sendCustomMessage(
      type = 'graphdata',
      message = links)
  })
  
  # output$table = renderDataTable(getValues)
  
  
  #output$nodeCountText <- renderText({
  #  paste("You have selected ", input$nodecount)
  # })
  
}

shinyApp(ui, server)
