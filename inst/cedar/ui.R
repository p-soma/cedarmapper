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
      menuItem("Graph",      tabName = "graph",      icon = icon("eye"))
    ),
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
                       selectInput("partitionCountSelection", label = "Number of Partitions", 
                                   choices = partitionCountChoices, selected = 4),  
                       selectInput("overlapSelection", label = "Partition Overlap (percent)", 
                                   choices = c(0:13) * 5  + 10, selected = 50),
                       selectInput("clusterIndexSelection", label = "Cluster Index",
                                   choices = clusterIndexChoices, selected = 1),
                       actionButton("runMapper", "Calculate Mapper"),
                   # temporary - delete this soon. 
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "data",
              dataTableOutput('dataset')
      ),
      
      tabItem(tabName = "graph",
              fluidRow(
                  box(plotOutput("plot1", width = 600),
                      title = renderText("partitionCount"),
                      solidHeader = TRUE,
                      width=12)
              )
      )
    )
    
) # end of dashboard body
) # end of dashboard page