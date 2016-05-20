
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "cedar.css")
      ),
      
      fluidRow(h3("CedarProject: Node Data")),
      # selectInput("col", "Colour:",
      #            c("white", "light green", "red", " light blue", "purple")),
      fluidRow(
      column(2,wellPanel(
             h3("data statistics here"),
             p(renderText(output$dataSpecs)),
             p("graph statistics"),
             p("group information")
      )),  # end sidebar
      column(10,
        tabsetPanel(id="tabs",
        tabPanel("Options",
                 column(4,
                 wellPanel(
                   selectInput("dataSelection", label = "Data", 
                               choices = dataChoices, selected = 1),
                   selectInput("lenseFunctionSelection", label="Lense Function", 
                               choices = lenseChoices, selected = 1),
                   selectInput("filterVar", label = "Filtering Variable", 
                               choices = initVariableChoices, selected = 1),
                   selectInput("partitionCountSelection", label = "Number of Partitions", 
                               choices = partitionCountChoices, selected = 4),  
                   selectInput("overlapSelection", label = "Partition Overlap (percent)", 
                               choices = c(0:13) * 5  + 10, selected = 50),
                   selectInput("clusterIndexSelection", label = "Cluster Index",
                               choices = clusterIndexChoices, selected = 1),
                   actionButton("runMapper", "Calculate Mapper")
                 )),
                 column(8,
                        p("cluster info"))),
        tabPanel("Graph",
              fluidRow(
                column(6,selectInput("selectedVar", label = "Variable", 
                    choices =  initVariableChoices, selected = 1)
                    ),
                column(6,
                       h4("Selected Nodes"),
                       p(textOutput("nodeListText"))
                    )
              ),
              fluidRow(cedarGraphOutput("cgplot","100%",500)),
              fluidRow(
                column(3,actionButton("grp1set", "Set Group 1"),
                       p(textOutput("group1Count"), " nodes")
                       ),
                column(3,actionButton("grp2set", "Set Group 2"),
                       p(textOutput("group2Count"))
                       ),
                column(4, 
                       tableOutput("hypTestTable")
                      #actionButton("runTest", "Compare Groups"),
                    )
              )
          ), # end graph panel        
        tabPanel("data",
                 fluidRow(
                   wellPanel(
                     dataTableOutput('selectedData')
                   )
                 )
          ), # end table panel
        tabPanel("Clusters",p("cluster graphs here")),
        tabPanel("Info",
             uiOutput("selectedVariable"),
             #uiOutput("nodeListInput"), 
             #uiOutput("nodeValuesInput"),
             conditionalPanel(
               condition="(input.nl)",
               plotOutput("selectedHist")
             )
        ) 
      )
    ))
  
)) 
  