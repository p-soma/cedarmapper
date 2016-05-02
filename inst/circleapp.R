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

gm =   makegraphmapper(circle_data(1, 60), circle_lense, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap")
graph_nodes = nodePrep(gm,"Y")
graph_links = linkPrep(gm)

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


varchoices = names(gm$d)
clusterIndexChoices = c( "gap", "all", "alllong", "kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew","friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2",  "beale", "ratkowsky", "ball", "ptbiserial", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")

# jsCode <- "shinyjs.pageCol = function(params){$('body').css('background', params);}"

ui <- 
  fluidPage(

    h3("CedarProject: Node Data"),
    shinyjs::useShinyjs(),
    # extendShinyjs(text = jsCode),
    # selectInput("col", "Colour:",
    #            c("white", "light green", "red", " light blue", "purple")),
    
    fluidRow(
      column(6, 
             wellPanel(
               
               selectInput("randomizeSelect", label="Data Type", 
                           choices= list("uniform", "random"),
                           selected = 1),
               
               selectInput("clusterIndex", label = "Cluster Index",
                            choices = clusterIndexChoices, selected = 1),
               
               selectInput("selectedVar", label = "Variable", choices = varchoices, selected = 1),

               actionButton("redraw", "Redraw"),
               
               wellPanel(h4("Mapper Output"), cedarGraphOutput("cedargraph")),
               div("Group 1:", textOutput("group1list")),
               div("Group 2:", textOutput("group2list"))
               
              ),
             actionButton("grp1set", "Set Group 1"),
             actionButton("grp2set", "Set Group 2"),
             actionButton("runTest", "Compare Groups")
            
      ),
      column(6, wellPanel(
        uiOutput("selectedVariable"),
        uiOutput("nodeListInput"),
        uiOutput("nodeValuesInput"),
        conditionalPanel(
          condition="(input.nl)",
          
          tabsetPanel(
            tabPanel("X", plotOutput("nodePlotX")),
            tabPanel("Y", plotOutput("nodePlotY"))
            # tabPanel("Table", tableOutput("nodeTable"))
          )
        ),
        p("Compare Groups:"), 
        textOutput("hypTest")
      )
      
    )
  )
)



########
server <- function(input, output, session) {
  
  output$varianceX <- renderUI({p(var(getValues()["X"]))})
  output$varianceY <- renderUI({p(var(getValues()["Y"]))})

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
  
  # GRAPH WIDGET
  # graph_links and graph_nodes are prepped from a graphmapper object
  # created from input data
  # at top of this application
  output$cedargraph <- renderCedarGraph({
    cedarGraph(graph_links, graph_nodes,"500","100%")
  })
 

 # make a plot output for all variables 
 for (vname in varchoices) {
   local({
     local_vname <- vname
     
     output[[paste0("nodePlot", local_vname)]] = renderPlot({
          v =  getValues()[local_vname]
          qplot(v,
            main = paste0("Histogram of ", local_vname), 
            xlab = "Data Values",
            fill=I("blue"), 
            col=I("black"), 
            alpha=I(.2))
         })
  })

}
#  output$nodePlotY = renderPlot({
#      v = getValues()["Y"]
#      qplot(v,
#            main = "Histogram of Y", 
#            xlab = "Data Values",
#            fill=I("blue"), 
#            col=I("black"), 
#            alpha=I(.2))
#    })
  
  output$nodeListInput <- renderUI({
    textInput("nl","selected nodes", paste(getNodeList(), sep=",", collapse = ","))
  })
  
 # output$nodeValuesInput <- renderUI({
 #    textInput("vl","selected values", paste(getValues(), sep=",", collapse = ","))
 #  })
  
  
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
      selected_nodes <- graph_nodes[graph_nodes$name %in% ns,]}
    n = as.vector(selected_nodes["name"])   #  "name" is 'nodeid' as used in the node prep script
    # print(n)  # debug
    return(n)
  })
  
  ## get node values from the getNodes()
  getValues <- reactive({
    node_ids = getNodes()
    
    if( is.null(node_ids) || nrow(node_ids)==0 ){
      return(0) }
    datarows = nodelistdata(as.numeric(unlist(node_ids)),gm) 
    # n = gm$nodes[as.numeric(unlist(node_ids))]
    # datarows = ldply(n, data.frame)
    # return all the rows now for this one variable 
    # get(selectedVar(),datarows)
    return(datarows)
  })
  
  
}

shinyApp(ui, server)
