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
default_gm  = graphmapper(x = datasets[["Diabetes"]], simple_lense, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap",  lenseparam = names(datasets[["Diabetes"]])[1])


####### server
shinyServer(function(input, output, session) {
  startUpState <- TRUE
  gm <- default_gm
  d <- datasets[[1]]

    ### FIRST must select data
  selectedDataSet <- observe({
    print(input$dataSelection)
    if(is.null(input$dataSelection)){
      d <<- datasets[[1]] }
    else {
      d <<- datasets[[input$dataSelection]] }
    print(paste0("1st row of",names(d)," is ", d[,1]))
    updateSelectInput(session, "selectedVar",choices = names(d))
    updateSelectInput(session, "filterVar",  choices = names(d))
    # TODO: is this needed?
    return(d)
  })
  
  # filterVar ==> lenseparam
  filterVar <- reactive({
    if(is.null(input$filterVar)) v = names(selectedDataSet())[1]
    else v = input$filterVar
    return(v)
  })
  
  # selectedVar ==> color and data exploration variable 
  selectedVar <- reactive({ 
    if(is.null(input$selectedVar)){ v = names(selectedDataSet())[1]}
    else{ v = input$selectedVar}
    return(v)
  })
  
  # TODO temp disable this and use hard coded variable names for testing
  # selection button dynamic to gm object

   calc_gm <-eventReactive(input$runMapper, {
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
            lenseparam = filterVar(),
            progressUpdater = updateProgress)
         
         # updateTabsetPanel(session, "tabs", selected = "graph")
         return(gm)
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
  output$selectedData = renderDataTable({d})
  
  output$cgplot <- renderCedarGraph({
    input$runMapper
    graphdata <-isolate(
      list(graph_nodes = nodePrep(calc_gm(),selectedVar()), graph_links = linkPrep(calc_gm()))
    )
    print("rendering graph")
    cedarGraph(graphdata$graph_links, graphdata$graph_nodes,"500","500")
  })
  
    
  # output$selectedHist = renderPlot(
  #  {
      # get data from selected nodes, for selected variable
  #    hist(getValues(), main=NULL, xlab=NULL, ylab=NULL,axes=FALSE,labels=TRUE,col="gray")
  #   })
  
  # display the list of selected nodes
  # TODO: this is not needed for display, but may be needed for tracking selections
  output$nodeListInput <- renderUI({
    textInput("nl","selected nodes", paste(getNodeList(), sep=",", collapse = ","))
  })
  
  # collect values from selected nodes into a text input button
  # currently not displayed, and not needed 
  #output$nodeValuesInput <- renderUI({
  #  textInput("vl","selected values", paste(getValues(), sep=",", collapse = ","))
  # })

  output$sparkHist = renderPlot({
     if(!is.null(selectedVar())) sparkline( density(getValues(), bw="nrd",kernel="gaussian")$y)
   })
  
  ### run hypothesis test
  # get data from two inputs containing the groups
  
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

  output$group1Count <- renderText({ group1() })
  output$group2Count <- renderText({ group2() })
  
  # on button click, render table
  hypTable <- eventReactive(input$runTest, {

    t = kstable(gm, c("group1", "group2"))
    print(t)
    return(t)
  })
  
  output$hypTestTable <- renderTable({
    group1()
    group2()
    print('testing groups =- ')
    print(has.groups(gm))
    if(!has.groups(gm)) return(NULL)
    
    print("running table on groups = ")
    print(gm$groups)
     kstable(gm)
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
  
  output$nodeListText = renderText({ getNodeList()})
  # get nodes from the graph_nodes data structure
  # not from the gm graphmapper object
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
  # TODO use R functions from GraphMapper object to get data
  # nodes = ?getnodes?(node_ids)
  # nodedata(gm,nodes, selectedVar())
  getValues <- reactive({
    node_ids = getNodeList()
    ##### TODO Warning: Error in if: missing value where TRUE/FALSE needed
    if( is.null(node_ids)   ) { return(0) }
    if( length(node_ids)==0 ) { return(0) }
    datarows = gm$d[unique(unlist(gm$nodes[names(gm$nodes) %in% node_ids])), selectedVar()]
    # get(selectedVar(),datarows)
    return(datarows)
  })
  
})# end of shinyServer