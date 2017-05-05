# CEDAR application
# UI.R
# user interface that compliments the server.R file

library(shiny)
library(shinydashboard)

tags$head(
  tags$script('var dimension = [0, 0];
      $(document).on("shiny:connected", 
      function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
        });

      $(window).resize(function(e) {
      dimension[0] = window.innerWidth;
      Shiny.onInputChange("dimension", dimension);
      });
'))


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
    tags$div(align="center", style="position: absolute; bottom: 10%; left:50%; margin-left:-25px;",
    tags$img(src="http://cabs.msu.edu/toolkit/images/helmet/gif/Spartan-helmet-White-150-pxls.gif", height=50)
    ),
    tags$hr() ,
    
    tags$div(class="user-panel",
        conditionalPanel("input.runMapper",             
        selectInput("selectedVar", label = "Color by:", choices =  initVariableChoices),
        
        conditionalPanel("output.selectedIsCategorical",
          selectInput("categoricalVar", label = "Category:", choices =  list('a','b','c'))
        ),
        p(
          actionButton("grp1set",   "Grp 1"),
          actionButton("grp1remove","Rm"),
          actionButton("grp1clear", "Clear")
        ),
        p(textOutput("group1Count", inline = TRUE), " nodes"),
        p(
          actionButton("grp2set",   "Grp 2"),
          actionButton("grp2remove","Rm"),
          actionButton("grp2clear", "Clear")
        ),
        p(textOutput("group2Count", inline = TRUE), " nodes"),
        p(class="shiny-input-container",
               actionButton("runTest", "Compare Groups")),
        p(class="shiny-input-container", 
               actionButton("showHist", "Show selected"))
            
       )   
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
            bsCollapse(id = "uploadDataCollapse",  
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
          checkboxInput('factorTextData', 'Select only numeric data', FALSE),
          checkboxGroupInput("selectedColumns", label = "Select columns to use", choices=initVariableChoices, selected = initVariableChoices, inline = TRUE), 
          dataTableOutput('dataset')
      ),
      # First tab content
      tabItem(tabName = "params",
          fluidRow( # all mapper params
            column(width=4,
            box( title="Mapper Parameters", width=NULL, background ="light-blue",
              h3("Data set:", textOutput("dataname",inline=TRUE),color="light-blue"),
              checkboxInput("normalizeOption", "Normalize Data?", value = TRUE, width = NULL),
              checkboxInput("equalizeOption", "Equalize Data?", value = FALSE, width = NULL),
              sliderInput("binCountSelection", label = "Cluster Bin Count", 
                          min=min(3),max=max(50), value=10,
                          step=1)  
              )
            ),
            column(width=4,
              box(width=NULL, title="Mapper", 
                  actionButton("runMapper", "Calculate Mapper"))
            )
          ),
          
          fluidRow(
            column(width=4,
              box( title="Dimension 1", width=NULL, background ="navy",
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
                             choices = c(0:13) * 5  + 10, selected = 50)
              )
            ),
            
            column(width=4,
              box(title="Dimension 2", width=NULL, background ="navy", 
                  selectInput("lense2FunctionSelection", label="Lense Function", 
                             choices = c("none", lenseChoices), selected = 1),
                  
                  conditionalPanel(condition = "input.lense2FunctionSelection != 'none'",
                    conditionalPanel(condition = "input.lense2FunctionSelection == 'Projection'",
                                   selectInput("lense2filterVar", label = "Filtering Variable", 
                                               choices = initVariableChoices, selected = 2)
                    ),
                    conditionalPanel(condition = "input.lense2FunctionSelection != 'none' && input.lense2FunctionSelection != 'Projection'",
                                   uiOutput("lense2ParamInput") 
                                   # textInput("lenseParam", label = lenses[input.lenseFunctionSelection,]$desc)}
                    ),
                  
                    sliderInput("lense2partitionCountSelection", label = "Number of Partitions", 
                              min=min(partitionCountChoices),max=max(partitionCountChoices), value=4,
                              step=1),  
                    selectInput("lense2overlapSelection", label = "Partition Overlap (percent)", 
                              choices = c(0:13) * 5  + 10, selected = 50)
                  
                  )
              )
            )
          )# end row
      ),
      

      tabItem(tabName = "graph",
         fluidRow(
           column(width=10,
              #box(title = NULL,
              #    width=NULL,
              tags$div(id="graphcontainer",
                  conditionalPanel("input.runMapper", cedarGraphOutput("cgplot","100%","100%")),
                  conditionalPanel("!input.runMapper",h4("Please select parameters and calculate mapper to see graph"))
              )
            ),
           column(width=2,
              ## TODO add list of input parameters in smaller table    
              box(uiOutput("gmParameters"), title="Parameters", width=NULL, background="light-blue"), 
              
              #valueBox(uiOutput("graphNodeCount"), 
              #    subtitle="Nodes", icon = icon("circle-o"),
              #    width=NULL, color="light-blue"),
              
            
              bsModal("hypothesisTest", "Hypothesis Testing of data in nodes by Group", "runTest",size = "large",
                      tableOutput("hypTestTable"),
                      tableOutput("varianceTable")
              ),
              bsModal("valHist", "Histogram of Selected Nodes", "showHist", size = "large", plotOutput("nodeHist1"))
                     # plotOutput("nodeHist2"))
                 

 
          )
         ) # end of row1
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
      
      
    ) # end of tabitems
    
) # end of dashboard body
) # end of dashboard page
