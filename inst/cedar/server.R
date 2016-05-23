# CEDAR application, using R Shiny
# server.R  
# data and processing

#' @import htmlwidgets
#' @import cedargraph
#' @import shiny
#' @import plyr
#' @import ggplot2


library(htmlwidgets)
library(cedargraph)
library(cedar)
library(shiny)
library(plyr)

# see the file global.R, which creates starting values for each new session of this shiny app 

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
  
  dataRows <- reactive({ nrow(gm$d)})
  # this sends an array of means for each node,
  # from the gm$d data frame column
  # of the selected variable to Shiny via the session object
  
  # selectedVar ==> color and data exploration variable 
  selectedVar <- reactive({ 
    if(is.null(input$selectedVar)){ v = names(selectedDataSet())[1]}
    else{ v = input$selectedVar}
    return(v)
  })
  
  # Server->Javascript
  # when a new variable is selected in the selectVar select box input,
  # collects mean values of that variable from all nodes, and sends those values
  # to the cedar_graph widget via sendCustomMessage, and the javascript is updated
  # there and the (D3.js) nodes are updated and recolored, etc
  observe({
    input$selectedVar
    if (selectedVar() %in% names(gm$d)) {
      vals =  nodePrep(gm, selectedVar())$values
      session$sendCustomMessage(type='nodevalues',message = vals)
    }
  })
  
  # javascript => server
  # input$nodelist is created by the Javascript HTMLWidget on selection events
  # ( e.g. currently using the D3.js dispatch feature)
  # this wraps that input in reactive context to return 0 when no selection made 
    # note this is changed on Shiny.onInputChange("nodelist", nodelist);

  # when group 1 button is clicked, get the currently selected nodes
  # and store the list in the graphmapper object 
  observeEvent(input$grp1set,{
    gm$groups[["group1"]] <<- as.numeric(input$nodelist)
      #getNodeList())
  })
  
  # when 'group 2' button is clicked, return currently selected nodes
  # and store the list in the graphmapper object 
  observeEvent(input$grp2set,{
    gm$groups[["group2"]] <<- as.numeric(input$nodelist)
  })
  
  
  group1Length <- reactive({
    input$grp1set
    length(unlist(gm$groups[["group1"]]))
  })
  
  group2Length <- reactive({
    input$grp2set
    length(unlist(gm$groups[["group2"]]))
  })
  
  

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
  
  testy <- eventReactive(input$runTest,{
           print('testing groups =- ')
           #print(has.groups(gm))
           # if(!has.groups(gm)) return(NULL)
           # print("running table on groups = ")
           # print(gm$groups)
          updateTabItems(session, "tabs", selected = "results")
          return(kstable(gm))
            } )
  
  output$hypTestTable <- renderTable({
    testy()
  }) 
  
  output$varianceTable <- renderTable({varTable(gm)})
  
  ########### outputs
  output$dataname    <- renderText(input$dataSelection)
  output$datarows    <- renderText({dataRows()})
  output$dataset     <- renderDataTable({d})
  output$nodeCount   <- renderText({prettyNum(length(gm$nodes))})
  output$selectedNodeCount <- renderText({ prettyNum(length(as.numeric(input$nodelist)))})
  output$group1Count <- renderText({group1Length()})
  output$group2Count <- renderText({group2Length()})
  
  # output from ACE code editor
  # TODO : secure this function; check session$host=='localhost'?
  output$eval_output <- renderPrint({
    input$eval
    return(isolate(eval(parse(text=input$rcode))))
  }) 
  
  # output$sessionInfo <- renderPrint({ session })
  
})
