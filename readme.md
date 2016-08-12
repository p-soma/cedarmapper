CEDAR project
------

This is a  Shiny app for the MSU CEDAR project of Dr. Jose Perea. 

It employs the concurrent HTMLWidget visualization project [CedarGraph](https://gitlab.msu.edu/billspat/cedargraph).  

Installation  (DRAFT)
----

Installation requires the R library 'devtools'  

     install.packages('devtools')

Clone and install both https://gitlab.msu.edu/billspat/cedargraph and  https://gitlab.msu.edu/billspat/cedar

Once cloned open each in R or R studio.   ( can use  *.Rproj files for R studio).    Then, for each, use 

devtools::install()

To install the libraries.  

Cedar requires the following libraries to be installed, which should install with the command above.  

    cluster (>= 2.0.0),
    NbClust (>= 3.0),
    plyr(>= 1.8.0),
    igraph(>= 1.0),
    ggplot2(>= 2.0),
    shiny (>= 0.13),
    shinydashboard (>= 0.5.1),
    htmlwidgets(>= 0.6), 
    V8(>= 1.0),
    DT(>= 0.1),
    shinyAce(>= 0.2.1),
    shinyBS(>= 0.60)
    
Use
---

Open the Cedar project folder in Rstudio, or open the cedar.Rproj file.    

To run examples in R, open examples folder and run our source the examples.   Graphs are displayed using iGraph. 

To run Cedar shiny application, and either use command 

  shiny::runApp('inst/cedar')
  
or open on of global.R, server.R or ui.R  in inst/cedar, and click the "run app" green triangle button in Rstudio.  Requires Rstudio to run.  

Common source of problems are libraries that have not been install.   Identifying those for new users will require more testing. 

Data
----

Appropriate data is from Reaven, G. M. and Miller, R. G. (1979). An attempt to define the nature of chemical diabetes using a multidimensional analysis. Diabetologia 16, 17-24,  This Rdata file was taken from the  'locfit' package https://cran.r-project.org/web/packages/locfit/ and put here for convenience.   

