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

source("R/nodeFunctions.R")
source("R/circleFunctions.R")
source("R/widget.R")

### DATA
data(chemdiab)
chemdiab  = subset(chemdiab, select = -c(cc))
circle = circle_data(r=1, n=100)
randomcircle = circle_data(r=1, n=100, randomize = TRUE)

###

### OBJECT
gm =   makegraphmapper(x = chemdiab, simple_lense, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap", "rw")
# gm =   makegraphmapper(circle_data(1, 60), circle_lense, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap")

### 
#graph_nodes = nodePrep(gm, "rw")
#graph_links = linkPrep(gm)

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
getSelectedValues = function(gm, node_id_list_str, varName){
  # TODO: check that varName is column in gm$d data
  node_ids = str2vec(node_id_list_str)
  nodes = gm$nodes[node_ids]
  # collapse list of nodes (data frames) into single data frame
  datarows = ldply(n, data.frame)
  # return one column from above
  return(get(varName,datarows))
}

####### starting values
# varchoices = names(gm$d)
clusterIndexChoices = c( "gap", "all", "alllong", "kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew","friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2",  "beale", "ratkowsky", "ball", "ptbiserial", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")
partitionCountChoices = c(3:10)



########
server <- function(input, output, session) {
  
  # this sends an array of data from the gm$d data frame column
  # of the selected variable to Shiny via the session object
  observe({
    if (! is.null(input$selectedVar)){
      if (input$selectedVar %in% names(gm$d)) {
        session$sendCustomMessage(type='nodevalues',
                              message = gm$d[[input$nodeValuesName]])
    }}
  })
  
  output$dataSpecs  <- renderText({
    paste0(input$dataSelection, " with ", nrow(gm$d), " rows and ", ncol(gm$d), " columns; ", "using variable ", input$selectedVar)
  })
  
  output$varianceX <- renderUI({p(var(getValues()["rw"]))})
  output$varianceY <- renderUI({p(var(getValues()["fpg"]))})

  #observeEvent(input$redraw,
  #  session$sendCustomMessage(type = 'rerender',message = 0),
  #  cedarGraph(graph_links, graph_nodes,"500","100%")
  # )
  
  # observeEvent(input$col, {
  #  js$pageCol(input$col)
  # })
  
  selectedVar = reactive({ 
    v = input$variableselect
    return(v)})
  
  output$selectedVariable <- renderText({selectedVar()})

  ### debugging only
  observe({
    print(cat(as.numeric(input$nodelist)))
    if (!is.null(input$nodelist)) {
      print("class:")
      print(class(getNodeList()))
      print(1 %in% getNodeList())
    }
  })
  
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
   
    input$redraw
    graphdata <- isolate(
      list(graph_nodes = nodePrep(gm,input$selectedVar), graph_links = linkPrep(gm))
    )

    cedarGraph(graphdata$graph_links, graphdata$graph_nodes,"500","500")
  })
 
 output$selectedHist = renderPlot(
   {
     # get data from selected nodes, for selected variable
     d = gm$d[[names(gm$d) ]]
     hist(d, main=NULL, xlab=NULL, ylab=NULL,axes=FALSE,labels=TRUE,col="gray", 
          width=200, height=50)
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
  # depends on global var 'gm' 
  output$hypTest <- renderText({
    
    getNodeRows <- function(node_id_list){
      node_ids = str2vec(node_id_list)
      return(gm$d[unlist(gm$nodes[node_ids]),])
    }

    nodes1 = getNodeRows(groupSets()[[1]])
    nodes2 = getNodeRows(groupSets()[[2]])
    n1 = get(getSelectedVar(),nodes1)
    n2 = get(getSelectedVar(),nodes2)
    print(n1)
    print(n2)
    x = ks.test(n1,n2)
    paste0("Statistic: ", x$statistic, " P-value: ", x$p.value)
  })
  
  
  # NODE FUNCTIONS
  
  # don't need this; just use the input value
  getSelectedVar <- reactive({
    return(input$selectedVar)  
  })
  
  getNodeList <- reactive({
    thisnodelist <- NULL
    if (!is.null(input$nodelist)) {
      print(input$nodelist)
      thisnodelist <- as.numeric(input$nodelist)
      print(thisnodelist)
    }
    print("getNodeList")
    
    return(thisnodelist)
  })
  
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
  ## get node values from the getNodes()
  getValues <- reactive({
    node_ids = getNodeList() # getNodes()
    
    if( is.null(node_ids) || nrow(node_ids)==0 ){
      return(0) }
    
    datarows = gm$d[unique(unlist(gm$nodes[names(gm$nodes) %in% ns])), ]
        
    # get(selectedVar(),datarows)
    return(datarows)
  })
  
  
}

###############################
# jsCode <- "shinyjs.pageCol = function(params){$('body').css('background', params);}"

ui <- 
  fluidPage(
    h3("CedarProject: Node Data"),
    shinyjs::useShinyjs(),
    # extendShinyjs(text = jsCode),
    # selectInput("col", "Colour:",
    #            c("white", "light green", "red", " light blue", "purple")),
    tabsetPanel(
      tabPanel("Graph",
               
               fluidRow(
                 column(2, 
                        wellPanel(
                          selectInput("nodeValuesName", label = "Node Values",
                                      choices = datachoices,
                                      selected = 1),
                          
                            selectInput("dataSelection", label = "Data", 
                                        choices = c("Diabetes", "Fixed Circle","Random Circle"), selected = 1),
                            selectInput("filterFunctionSelection", label="Filtering Function", 
                                        choices = c("SimpleLense","KernelDensity", "PCA"),selected = 1),
                            selectInput("partitionCountSelection", label = "Number of Partitions", 
                                        choices = c(2:15), selected = 3),  
                            selectInput("overlapSelection", label = "Partition Overlap (percent)", 
                                          choices = c(0:13) * 5  + 10, selected = 50),
                            selectInput("clusterIndexSelection", label = "Cluster Index",
                                        choices = clusterIndexChoices, selected = 1),
                            actionButton("runMapper", "Calculate Mapper"),
                            hr(),
                            selectInput("selectedVar", label = "Variable", choices =  names(gm$d), selected = 1),
                          
                          hr(),
                          actionButton("grp1set", "Set Group 1"),
                          p("Group 1:", p(textOutput("group1list"))),
                          actionButton("grp2set", "Set Group 2"),
                          p("Group 2:", p(textOutput("group2list"))),
                          actionButton("runTest", "Compare Groups"),
                          h4("Compare Groups:"), 
                          p(textOutput("hypTest"))
                 
                        )
                    ),  # end sidebar
                
                 column(10,
                        h4("Mapper Output"), 
                        # uiOutput("cedarGraphUI")
                        # cedarGraphOutput("cedargraph",1000,500),
                        cedarGraphOutput("cgplot","100%",500),
                        fluidRow(
                          column(4,
                            p("Data Set ", textOutput("dataSpecs", inline=TRUE)),
                            p("Mapper options: ", "X")
                            ),
                          column(2, 
                                 conditionalPanel(
                                   condition="(input.nl)",plotOutput("selectedHist"))
                                 ),
                          column(4, 
                            actionButton("redraw", "Redraw")
                            )
                        )
                 ))
               ),
      tabPanel("histograms",
               fluidRow(
                 wellPanel(
                   uiOutput("selectedVariable"),
                   uiOutput("nodeListInput"),
                   uiOutput("nodeValuesInput"),
                   conditionalPanel(
                     condition="(input.nl)",
                     plotOutput("nodePlotrw")
                   )
                 )
               )
       ) # end table panel
    )
  )




shinyApp(ui, server)



##### saved code
#  output$nodePlotY = renderPlot({
#      v = getValues()["Y"]
#      qplot(v,
#            main = "Histogram of Y", 
#            xlab = "Data Values",
#            fill=I("blue"), 
#            col=I("black"), 
#            alpha=I(.2))
#    })

# DON'T make a plot output for all variables; unstainable
#for (vname in varchoices) {
#   local({
#    local_vname <- vname
#     output[[paste0("nodePlot", local_vname)]] = renderPlot({
#          v =  getValues()[local_vname]
#          qplot(v,
#            main = paste0("Histogram of ", local_vname), 
#            xlab = "Data Values",
#           fill=I("blue"), 
#            col=I("black"), 
#            alpha=I(.2))
#         },height = 75, width=200)
#  }