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
      menuItem("Data",       tabName = "data",       icon = icon("table")),
      menuItem("Parameters", tabName = "params",     icon = icon("sliders")),
      menuItem("Graph",      tabName = "graph",      icon = icon("eye")),
      # menuItem("Results",    tabName=  "resulttable",    icon = icon("bar-chart")),
      menuItem("Console",    tabName = "console",    icon = icon("terminal"))
    ),
    tags$div(align="center", style="position: absolute; bottom: 10%;
    left:50%; margin-left:-25px;",
      tags$img(src="http://cabs.msu.edu/toolkit/images/helmet/gif/Spartan-helmet-White-150-pxls.gif", height=50)
    )
  ),
  
#############
dashboardBody(
    includeCSS("cedarcustom.css"),
  
    tabItems(
      # Second tab content
      tabItem(tabName = "data",
          fluidRow(
            box(width=3,selectInput("dataSelection", label = "Select Dataset", choices = dataChoices, selected = 1)), 
            valueBox(width=2, subtitle="rows", value = textOutput("dataRowCount",inline=TRUE),color='black'),
            valueBox(width=2, subtitle="variables", value = textOutput("dataVarCount",inline=TRUE),color='black'),
            
            box(width=5, 
            bsCollapse(id = "collapseExample", open = "Panel 1", 
            bsCollapsePanel("Click to Upload Data",
              box(                                                                   
            
                fileInput('file1', 'Choose file to upload',accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain',
                        '.csv','.tsv')),
                textInput('newDataName', "Name", placeholder = "name your data"),
                checkboxInput('header', 'Header', TRUE),
                radioButtons('sep', 'Separator',
                         c(Comma=',',Semicolon=';', Tab='\t'),','),
            radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'"'),
            actionButton("uploadDataAction", "Upload")
            ),# end of upload box 
            style = "primary") 
          ))),
          dataTableOutput('dataset')
      ),
      # First tab content
      tabItem(tabName = "params",
          fluidRow(
            column(width=4,
              box( title="Mapper Parameters", width=NULL, background ="light-blue",
                 h3("Data set:", textOutput("dataname",inline=TRUE),color="light-blue"),
                 checkboxInput("normalizeOption", "Normalize Data?", value = TRUE, width = NULL),
                 
                 selectInput("lenseFunctionSelection", label="Lense Function", 
                             choices = lenseChoices, selected = 1),
                 
                 conditionalPanel(condition = "input.lenseFunctionSelection == 'Projection'",
                        selectInput("filterVar", label = "Filtering Variable", 
                             choices = initVariableChoices, selected = 1)
                 ),
                 conditionalPanel(condition = "input.lenseFunctionSelection != 'Projection'",
                                   uiOutput("lenseParamInput") 
                                  # textInput("lenseParam", label = lenses[input.lenseFunctionSelection,]$desc)}
                 ),
                 
                 sliderInput("partitionCountSelection", label = "Number of Partitions", 
                             min=min(partitionCountChoices),max=max(partitionCountChoices), value=4,
                             step=1),  
                 selectInput("overlapSelection", label = "Partition Overlap (percent)", 
                             choices = c(0:13) * 5  + 10, selected = 50),
                 sliderInput("binCountSelection", label = "Cluster Bin Count", 
                             min=min(3),max=max(50), value=10,
                             step=1)  
              )
            ),
            column(width=4,
                   box(width=NULL, title="Mapper", 
                   actionButton("runMapper", "Calculate Mapper")),
                   box(width=NULL, title="Current Parameters",
                      valueBox(width=NULL, subtitle = "Partitions",     value=textOutput("gmPartitionCount",inline=TRUE),  color="black"),
                      valueBox(width=NULL, subtitle = "Percent Overlap",value=textOutput("gmOverlap",inline=TRUE),  color="black"),
                      valueBox(width=NULL ,subtitle = "Clustering Bins",  value=textOutput("gmBinCount",inline=TRUE),  color="black"),
                      valueBox(width=NULL ,subtitle = "Nodes",          value=textOutput("nodeCount",inline=TRUE),  color="black"),
                      downloadButton('downloadMapper', 'Download Mapper Rdata')

                   )

            ),
            column(width=4,
                   box(title="Cluster Info",width=NULL,"")
                       # selectInput("dataSelection", label = "Select Dataset", choices = dataChoices, selected = 1)),
                       
                       
                   )
                   
            )# end row
      ),
      

      
      tabItem(tabName = "graph",
         fluidRow(
           column(width=10,
              box(title = NULL,
                  width=NULL,
                  conditionalPanel("input.runMapper", cedarGraphOutput("cgplot","100%",500)),
                  conditionalPanel("!input.runMapper",h4("Please select parameters and calculate mapper to see graph"))
              )
            ),
           column(width=2,
              ## TODO add list of input parameters in smaller table    
              box(uiOutput("gmParameters"), 
                       title="Parameters", width=NULL, background="light-blue"),
              
              #valueBox(uiOutput("graphNodeCount"), 
              #    subtitle="Nodes", icon = icon("circle-o"),
              #    width=NULL, color="light-blue"),
              
              box(width=NULL,background="light-blue",
                  selectInput("selectedVar", label = "Color by:", 
                              choices =  initVariableChoices)
                  
              ),
              
              box(uiOutput("selectedNodeCount"),
                  actionButton("showHist", "Show selected"),
                  title= "Selected Nodes",
                  width=NULL, color="light-blue"),
              
              bsModal("valHist", "Histogram of Selected Nodes", "showHist", size = "large", plotOutput("nodeHist"))

 
          )
         ), # end of row1
         fluidRow(
           column(width=4,
               box(width=NULL,background="light-blue",
                   actionButton("grp1set",   "Set Group 1"),
                   actionButton("grp1remove","Remove Nodes"),
                   actionButton("grp1clear", "Reset"),
                   p(textOutput("group1Count", inline = TRUE), " nodes")
               )
           ),
           column(width=4,
                  box(width=NULL,background="light-blue",
                      div(style="display:inline-block",actionButton("grp2set",   "Set Group 2")),
                      div(style="display:inline-block",actionButton("grp2remove","Remove Nodes")),
                      div(style="display:inline-block",actionButton("grp2clear", "Reset")),
                      p(textOutput("group2Count", inline = TRUE), " nodes")
                 )
           ),
           column(width=2,
             box(width=NULL,background="black",
                 actionButton("runTest", "Compare Groups")
                 )
            )
         
         
         ,bsModal("hypothesisTest", "Hypothesis Testing of data in nodes by Group", "runTest",size = "large",
                  tableOutput("hypTestTable"),
                  tableOutput("varianceTable")
                  )
         ) # end row2
      ),
      
      tabItem(tabName="console",
              fluidRow(
                box(title="Enter R code",width=6,
                    p(actionButton("eval", "Click to Evaluate")),
                    hr(),
                    aceEditor("rcode", mode="r", value="names(gm)")
                    ),
                box(title="R Output",width=6,
                    verbatimTextOutput("eval_output") )
              )
      )
      #,
      #tabItem(tabName="resulttable",
      #        p("")
              #fluidRow(
              #  box(title="Hypothesis test",width=6,
              #      tableOutput("hypTestTable")),
              #  box(title="Group Data",width=6,
              #      tableOutput("varianceTable"))
              #)
      #)
      
    ) # end of tabitems
    
) # end of dashboard body
) # end of dashboard page