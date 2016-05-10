# example_app.R
# very basic example of using this widgnbcet in an RStudio Shiny app

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

#source("R/nodeFunctions.R")
#source("R/circleFunctions.R")
#source("R/widget.R")

####### useful functions

#'@export
varname2data <- function(nameOfVariable){
  # given the name of data set in a variable
  # nameOfVariable = "chemdiab"
  # this returns the actual data in the variable chemdiab
  # useful for selecting a data set from a drop down which returns a string
  # and copying into an object or known variable
  return(eval(parse(text=nameOfVariable)))
}

# this handy function converts a string list "1,2,4" to a vector c(1,2,4)
#'@export
str2vec <- function(list_str, sep = ","){
  as.numeric(unlist(strsplit(list_str,sep)))
}

#'@export
getSelectedValues = function(gmap, node_id_list_str, varName){
  # TODO: check that varName is column in gm$d data
  node_ids = str2vec(node_id_list_str)
  nodes = gmap$nodes[node_ids]
  # collapse list of nodes (data frames) into single data frame
  datarows = ldply(n, data.frame)
  # return one column from above
  return(get(varName,datarows))
}

### start up with default gm object
default_gm  = makegraphmapper(x = datasets[["Diabetes"]], simple_lense, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap",  lenseparam = names(datasets[["Diabetes"]])[1], progressUpdater = NULL)


####### server
shinyServer(function(input, output, session) {
  startUpState <- TRUE
  gm <- default_gm
  d <- datasets[[1]]

    ### FIRST must select data
  selectedDataSet <- observe({
    input$dataSelection
    if(is.null(input$dataSelection)){
      d <<- datasets[[1]] }
    else {
      d <<- datasets[[input$dataSelection]] }
    updateSelectInput(session, "selectedVar",choices = names(d), selected=1)
    updateSelectInput(session, "filterVar",  choices = names(d), selected=1)
    # TODO: is this needed?
    return(d)
  })
  
  # filterVar ==> lenseparam
  getfilterVar <- reactive({
    if(is.null(input$filterVar)) v = names(selectedDataSet())[1]
    else v = input$filterVar
    return(v)
  })
  
  # selectedVar ==> color and data exploration variable 
  selectedVar <- reactive({ 
    if(is.null(input$selectedVar)) v = names(selectedDataSet())[1]
    else v = input$selectedVar
    return(v)
  })
  
  # TODO temp disable this and use hard coded variable names for testing
  # selection button dynamic to gm object

   calc_gm <-eventReactive(input$runMapper, {
      #progress <- shiny::Progress$new()
      #progress$set(message = "Computing mapper", value = 0)
      #on.exit(progress$close())
      
      #updateProgress <- function(value = NULL, detail = NULL) {
      #  if (is.null(value)) {
      #    value <- progress$getValue()
      #    value <- value + (progress$getMax() - value) / 5 }
      #  progress$set(value = value, detail = detail)
      #}

         gm<<- makegraphmapper(x = selectedDataSet(), 
                      lensefun = simple_lense, 
                      partition_count=as.numeric(input$partitionCountSelection),
                      overlap = as.numeric(input$overlapSelection)/100.0, 
                      partition_method="single", 
                      index_method="gap",
                      lenseparam = filterVar() ) #,
                      #progressUpdater = updateProgress)
         
      
       })
  
  # this sends an array of means for each node,
  # from the gm$d data frame column
  # of the selected variable to Shiny via the session object
  observe({
      input$selectedVar
      if (selectedVar() %in% names(gm$d)) {
        vals =  nodePrep(gm, selectedVar())$values
        session$sendCustomMessage(type='nodevalues',message = vals)
      }
  })
  
  
  output$dataSpecs  <- renderText({
    paste0(input$dataSelection, " with ", nrow(gm$d), " rows and ", ncol(gm$d), " columns; ", "using variable ", selectedVar())
  })
  
  # TODO: should be observer on node selection
  # output$variance <- renderText({var(getValues()[selectedVar()])})

  output$selectedVariable <- renderText({selectedVar()})
  
  ### collect nodes when buttons are clicked
  group1 <- eventReactive(input$grp1set, {
    paste(getNodeList(), sep=",", collapse = ",")
  })
  
  group2 <- eventReactive(input$grp2set, {
    paste(getNodeList(), sep=",", collapse = ",")
  })
  
  # display the group node lists, if they are set
  output$group1list <- renderText({group1()})
  output$group2list <- renderText({group2()})
  
  # collect the two groups on button click
  groupSets <- eventReactive(input$runTest, {
    # TODO: add a test that the groups are set...
    list( group1(), group2())
  })
  
  ##########
  
  
  output$cgplot <- renderCedarGraph({
    input$runMapper
    graphdata <-isolate(
      list(graph_nodes = nodePrep(gm,selectedVar()), graph_links = linkPrep(gm))
    )
    print("rendering graph")
    cedarGraph(graphdata$graph_links, graphdata$graph_nodes,"500","500")
  })
  
    
  output$selectedHist = renderPlot(
    {
      # get data from selected nodes, for selected variable
      hist(getValues(), main=NULL, xlab=NULL, ylab=NULL,axes=FALSE,labels=TRUE,col="gray")
    })
  
  
  output$nodeListInput <- renderUI({
    textInput("nl","selected nodes", paste(getNodeList(), sep=",", collapse = ","))
  })
  
  output$nodeValuesInput <- renderUI({
    textInput("vl","selected values", paste(getValues(), sep=",", collapse = ","))
  })
  
  
  output$nodeTable = renderDataTable(data.frame(getValues()))
  
  ### run hypothesis test
  # get data from two inputs containing the groups
  output$hypTest <- renderText({
    getNodeRows <- function(node_id_list){
      node_ids = str2vec(node_id_list)
      return(gm$d[unlist(gm$nodes[node_ids]),])
    }
    
    nodes1 = getNodeRows(groupSets()[[1]])
    nodes2 = getNodeRows(groupSets()[[2]])
    n1 = get(getSelectedVar(),nodes1)
    n2 = get(getSelectedVar(),nodes2)
    x = ks.test(n1,n2)
    paste0("Statistic: ", x$statistic, " P-value: ", x$p.value)
  })
  
  
  # NODE FUNCTIONS
  
  # don't need this; just use the input value
  getSelectedVar <- reactive({
    return(input$selectedVar)  
  })
  
  getNodeList <- reactive({
    thisnodelist <- 0
    if (!is.null(input$nodelist)) {
      thisnodelist <- as.numeric(input$nodelist)
    }
    return(thisnodelist)
  })
  
  # get nodes from the graph_nodes data structure
  # not from the gm graphmapper object
  # not used
  getNodes <- reactive({
    ns <- getNodeList()
    selected_nodes <- NULL
    if( ! is.null(ns)) {
      gm$nodes
      selected_nodes <- graph_nodes[graph_nodes$name %in% ns,]}
    n = as.vector(selected_nodes["name"])   #  "name" is 'nodeid' as used in the node prep script
    # print(n)  # debug
    return(n)
  })
  
  # return rows of data based on selection
  # TODO rename getRows() and make seperate getValues(rows) function
  getValues <- reactive({
    node_ids = getNodeList()
    ##### TODO Warning: Error in if: missing value where TRUE/FALSE needed
    if( is.null(node_ids)   ) { return(0) }
    if( length(node_ids)==0 ) { return(0) }
    datarows = gm$d[unique(unlist(gm$nodes[names(gm$nodes) %in% node_ids])), input$selectedVar]
    # get(selectedVar(),datarows)
    return(datarows)
  })
  
})# end of shinyServer