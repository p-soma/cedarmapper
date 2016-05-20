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
      menuItem("Console",    tabName = "console",    icon = icon("terminal"))
    ),
    actionButton("runMapper", "Calculate Mapper"),
    p("Data:"),
    p(textOutput("dataname"))
  ),
  
#############
dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "params",
          fluidRow(
            box( background ="light-blue", title = "Parameters",width=4,
                 selectInput("dataSelection", label = "Data", 
                             choices = dataChoices, selected = 1),
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

            ) # end box
            ) # end row
      ),
      
      # Second tab content
      tabItem(tabName = "data",
              dataTableOutput('dataset')
      ),
      
      tabItem(tabName = "graph",
         fluidRow(
              box(title = " node graph",
                  solidHeader = TRUE, 
                  width=10,
                  cedarGraphOutput("cgplot","100%",500))
              ),
          fluidRow(
              box(title="Graph Options", width=2,background="light-blue",
                  selectInput("selectedVar", label = "Variable", 
                            choices =  initVariableChoices)),
              
              box(title="NodeCount",width=2,background="light-blue",
                  textOutput("nodeCount")),
              
              box(width=2,background="light-blue",
                  actionButton("grp1set", "Set Group 1"),
                  p(textOutput("group1Count", inline = TRUE), " nodes")
                  ),
              
              box(width=2,background="light-blue",
                  actionButton("grp2set", "Set Group 2"),
                  p(textOutput("group2Count", inline = TRUE), " nodes")
                  )
              
            
              )
      ),
      
      tabItem(tabName="console",
              fluidRow(
                box(title="Code",width=6,
                    p("textHere"))
              )
      )
      
    ) # end of tabitems
    
) # end of dashboard body
) # end of dashboard page