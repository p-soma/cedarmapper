# example_app.R
# very basic example of using this widget in an RStudio Shiny app

library(htmlwidgets)
library(cedargraph)
library(shiny)
data("cedarcircle")

ui <- basicPage(
  
  h3("Circle Data"),
  cedarGraphOutput("cedargraph"),
  
  # sliderInput("nodecount","Number of nodes:",min = 1,max = 50,value = 20),
  # p("node count"),
  #  textOutput("nodeCountText"),
  
  uiOutput("nodeListInput"),
  uiOutput("nodeValuesInput"),
  conditionalPanel(
    condition="(input.nl)",
    plotOutput("nodePlot")
  )
  
  # cedarGraphOutput("randomgraph")
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
    return(nodes)
  })
  
  getValues <- reactive({
    # TODO: create input$varname, e.g. from dropdown
    nodes = getNodes()
    if( is.null(nodes) || nrow(nodes)==0 ){
      return(NULL)
    }
    return(nodes$values)
  })
  
  output$cedargraph <- renderCedarGraph({
    # graph.data= randGraphData(n=20)
    #cedarGraph(graph.data$links, graph.data$nodes,250,250)
    cedarGraph(circle.links, circle.nodes,250,250)
  })
  
  output$nodePlot = renderPlot({
    barplot(height=as.vector(getValues()), names.arg = getNodeList())
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
