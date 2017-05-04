# example_app.R
# very basic example of using this widget in an RStudio Shiny app

library(htmlwidgets)
library(cedar)
library(shiny)

data("cedarcircleexample")

nodedata = list()
nodedata[["original"]]= circle.nodes$values
nodedata[["cosine"]] = cos(circle.nodes$values)
nodedata[["random"]] = (rnorm(length(circle.nodes$values),0,2))^2
datachoices = names(nodedata)
ui <- fluidPage(
    # put this in the source for now for testing
    # link to the message handler script
    #singleton(
    #    tags$head(tags$script(src = "message-handler.js"))
    # ),

    # graph set to 100% width to test non-pixel values, 500 px height
    h3("Circle Data"),
    cedarGraphOutput("cedargraph","100%","500"),
    selectInput("nodeValuesName", label = "Node Values",
        choices = datachoices,
        selected = 1),

    uiOutput("nodeListInput"),
    uiOutput("nodeValuesInput"),
    textOutput("nodeValues"),
    conditionalPanel(

        condition="(input.nl)",
        plotOutput("nodePlot")
   )

    # cedarGraphOutput("randomgraph")
)


server <- function(input, output, session) {


  observe({
    if (! is.null(input$nodeValuesName)){
      session$sendCustomMessage(type = 'nodevalues',
                              message = nodedata[[input$nodeValuesName]])}
  })

  output$nodeValues = renderText(
    {if (! is.null(input$nodeValuesName)) nodedata[[input$nodeValuesName]]
    else ""}
    )


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
      return(c(0))
    }
    return(nodes$values)
  })

  output$cedargraph <- renderCedarGraph({
    # graph.data= randGraphData(n=20)
    #cedarGraph(graph.data$links, graph.data$nodes,250,250)
    print ('render graph')
    print(circle.nodes)
    cedarGraph(circle.links, circle.nodes)
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
      cedarGraph(LinksDF =links, NodesDF = graph.data$nodes,250,500)
      session$sendCustomMessage(
        type = 'graphdata',
        message = links)
    })




  #output$nodeCountText <- renderText({
  #  paste("You have selected ", input$nodecount)
  # })

}

shinyApp(ui, server)
