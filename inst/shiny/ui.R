
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
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
                       plotOutput("selectedHist")
                     )
                   )
                 )
        ) # end table panel
      )
    )
)  
  