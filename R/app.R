# example_app.R
# very basic example of using this widget in an RStudio Shiny app

library(htmlwidgets)
library(cedargraph)
# library(cedar)
library(shiny)
data("cedarcircle")
source("cedarFunctions.R")

d = circle.data(r=1,n=100, randomize=FALSE)
d.partitions= cedar.partition(d, l = 4)
d.clusters  = cedar.clusters(d, d.partitions)
d.nodes     = cedar.nodes(d,d.clusters)

# nodedata = 
ui <- 
  fluidPage(
    h3("Circle Data"),
    fluidRow(
      column(6, wellPanel(
            
            cedarGraphOutput("cedargraph")
              )
      ),
      column(6, wellPanel(
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
    nodes <- NULL
    if( ! is.null(ns)) {
      nodes <- circle.nodes[circle.nodes$name %in% ns,]}
    n = as.vector(nodes["name"])
    print(n)
    return(n)
  })
  
  getValues <- reactive({
    # TODO: create input$varname, e.g. from dropdown
    node_ids = getNodes()
    if( is.null(node_ids) || nrow(node_ids)==0 ){
      return(NULL)
    }
   
    n = d.nodes[as.numeric(unlist(node_ids))]
    datarows = ldply(n, data.frame)
    return(datarows$X)
  })
  
  output$cedargraph <- renderCedarGraph({
    # graph.data= randGraphData(n=20)
    #cedarGraph(graph.data$links, graph.data$nodes,250,250)
    cedarGraph(circle.links, circle.nodes,250,250)
  })
  
  output$nodePlot = renderPlot({
    hist(getValues())
    # barplot(height=as.vector(getValues()), names.arg = getNodeList())
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
  
  
  
  
  #output$nodeCountText <- renderText({
  #  paste("You have selected ", input$nodecount)
  # })
  
}

shinyApp(ui, server)
