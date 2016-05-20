# CEDAR application
# UI.R
# user interface that compliments the server.R file

library(shiny)
library(shinydashboard)

dashboardPage(
  
dashboardHeader(title = "CEDAR"),
  
################
dashboardSidebar(
    
    sidebarMenu(
      id = "tabs",
      menuItem("Parameters", tabName = "params", icon = icon("dashboard")),
      menuItem("Data",       tabName = "data",       icon = icon("th")),
      menuItem("Graph",      tabName = "graph",      icon = icon("eye")),
      menuItem("Results",    tabName=  "results",    icon = icon("chart")),
      menuItem("Console",    tabName = "console",    icon = icon("terminal"))
    ),
    h3("Data:",textOutput("dataname",inline=TRUE))

  ),
  
#############
dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "params",
          fluidRow(
            column(width=4,
                box(title="DataSets",width=NULL,
                   selectInput("dataSelection", label = "Select Dataset", choices = dataChoices, selected = 1)),
                valueBox(width=NULL, subtitle="rows", value = textOutput("datarows",inline=TRUE))
                
            ),
            column(width=4,
              box( title="Parameters", width=NULL, background ="light-blue", 
                 selectInput("lenseFunctionSelection", label="Lense Function", 
                             choices = lenseChoices, selected = 1),
                 selectInput("filterVar", label = "Filtering Variable", 
                             choices = initVariableChoices, selected = 1),
                 sliderInput("partitionCountSelection", label = "Number of Partitions", 
                             min(partitionCountChoices),max(partitionCountChoices), 4),  
                 selectInput("overlapSelection", label = "Partition Overlap (percent)", 
                             choices = c(0:13) * 5  + 10, selected = 50),
                 selectInput("clusterIndexSelection", label = "Cluster Index",
                             choices = clusterIndexChoices, selected = 1)

              )
            ),
            column(width=4,
                   box( title="Mapper", witdh=NULL,
                        actionButton("runMapper", "Calculate Mapper"))

            )
            )# end row
      ),
      
      # Second tab content
      tabItem(tabName = "data",
              dataTableOutput('dataset')
      ),
      
      tabItem(tabName = "graph",
         fluidRow(
           column(width=10,
              box(title = " node graph",
                  solidHeader = TRUE, 
                  width=NULL,
                  cedarGraphOutput("cgplot","100%",500))
              ),
          column(width=2,
              box(title="NodeCount",
                  width=NULL,background="light-blue",
                 textOutput("nodeCount")),
              
              box(width=NULL,background="light-blue",
                  selectInput("selectedVar", label = "Color by:", 
                            choices =  initVariableChoices)),
              
              box(width=NULL,background="light-blue",
                  actionButton("grp1set", "Set Group 1"),
                  p(textOutput("group1Count", inline = TRUE), " nodes")
                  ),
              
              box(width=NULL,background="light-blue",
                  actionButton("grp2set", "Set Group 2"),
                  p(textOutput("group2Count", inline = TRUE), " nodes")
                  ),
              
              box(width=NULL,background="black",
                  actionButton("runTest", "Test"))
              )
         ) # end row
      ),
      
      tabItem(tabName="console",
              fluidRow(
                box(title="Code",width=6,
                    p("textHere"))
              )
      ),
      tabItem(tabName="results",
              fluidRow(
                box(title="test",width=12,
                    tableOutput("hypTestTable"))
              )
      )
      
    ) # end of tabitems
    
) # end of dashboard body
) # end of dashboard page