# example_app.R
# very basic example of using this widgnbcet in an RStudio Shiny app

#' @import htmlwidgets
#' @import cedargraph
#' @import shiny
#' @import plyr
#' @import ggplot2
#' @import shinyjs
#' @import NbClust

library(plyr)
library(htmlwidgets)
library(cedargraph)
# library(cedar)
library(shiny)
library(plyr)

# TODO: REPLACE THIS WITH DATA PREPARATION
# data("cedarcircle")
# source("cedarFunctions.R")

d = function(r=1, n=60, randomize=FALSE)

nodes = cedar::circlenodes(npoints = 500, lense_count = 4, lense_function=simple_lense, coordinate="Y")
gl= cedar::graphList(nodes)
graph_nodes = gl[[1]]
graph_links = gl[[2]]

getSelectedValues = function(nodes, node_id_list, varName)
{
  node_ids = strsplit(node_id_list,",")
  n = nodes[as.numeric(unlist(node_ids))]
  # collapse list of nodes (data frames) into single data frame
  datarows = ldply(n, data.frame)
  # return one column from above
  return(get(varName,datarows))
}

varchoices = names(d)
clusterIndexChoices = c( "gap", "all", "alllong", "kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew","friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2",  "beale", "ratkowsky", "ball", "ptbiserial", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")

# jsCode <- "shinyjs.pageCol = function(params){$('body').css('background', params);}"

ui <- 
  fluidPage(
    
    
    h3("CedarProject: Node Data"),
    shinyjs::useShinyjs(),
    extendShinyjs(text = jsCode),
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
  
 # output$selectedVariable <- renderText({selectedVar()})
                                        
  
  
  ## get node values from the getNodes()
  getValues <- reactive({
    node_ids = getNodes()
    if( is.null(node_ids) || nrow(node_ids)==0 ){
      return(0) }
    n = d.nodes[as.numeric(unlist(node_ids))]
    datarows = ldply(n, data.frame)
    # return all the rows now, 
    # get(selectedVar(),datarows)
    return(datarows)
  })
  
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

  # display the group members  
  output$group1list <- renderText({group1()})
  output$group2list <- renderText({group2()})
  
  # collect the two groups on button click
  groupSets <- eventReactive(input$runTest, {
    # test that the groups are set...
    list( group1(), group2())
  })
  
  


    
  getNodeList <- reactive({
    nl <- NULL
    if (!is.null(input$nodelist)) {
      print(input$nodelist)
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
  

  output$cedargraph <- renderCedarGraph({
    cedarGraph(graph_links, graph_nodes,"500","100%")
  })
  
  output$nodePlotX = renderPlot({
    v =  getValues()["X"]
    qplot(v,
          main = "Histogram of X", 
          xlab = "Data Values",
          fill=I("blue"), 
          col=I("black"), 
          alpha=I(.2))
  })
  
  output$nodePlotY = renderPlot({
      v = getValues()["Y"]
      qplot(v,
            main = "Histogram of Y", 
            xlab = "Data Values",
            fill=I("blue"), 
            col=I("black"), 
            alpha=I(.2))
    })
  
  output$nodeListInput <- renderUI({
    textInput("nl","selected nodes", paste(getNodeList(), sep=",", collapse = ","))
  })
  
 # output$nodeValuesInput <- renderUI({
 #    textInput("vl","selected values", paste(getValues(), sep=",", collapse = ","))
 #  })
  
  
  output$nodeTable = renderDataTable(data.frame(getValues()))
  
  ### run hypothesis test
  output$hypTest <- renderText({
    
    getNodeRows <- function(nodes, node_id_list){
      node_ids = strsplit(node_id_list,",")
      n = nodes[as.numeric(unlist(node_ids))]
      # collapse list of nodes (data frames) into single data frame
      return(ldply(n, data.frame))
    }
    
    nodes1 = getNodeRows(d.nodes, groupSets()[[1]])
    nodes2= getNodeRows(d.nodes, groupSets()[[2]])
    n1 = get(selectedVar(),nodes1)
    n2 = get(selectedVar(),nodes2)
    print(n1)
    print(n2)
    x = ks.test(n1,n2)
    paste0("Statistic: ", x$statistic, " P-value: ", x$p.value)
  })
  
}

shinyApp(ui, server)
