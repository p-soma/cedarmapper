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


Installing CEDAR and CEDARGraph
---

Developing CEDAR on Windows

Notes
Installing the pieces necesarry to develop a package requires more manual steps that simply installing a package from the 
R network CRAN.   When one installs a package from CRAN, all the dependencies are automatically installed. 
Not so for development packages.   


Download and Install R, select the latest version (3.3.1 as of this writing)

Download and install Rtools (version 3.4 )

https://cran.rstudio.com/bin/windows/Rtools/

Note: when installing don't need to the TCL/TK extras

Download and Install Rstudio latest version (no longer requires the preview version)

Start R or Rstudio

install 'devtools'  and include its dependencies with the following command:

install.packages('devtools', dependencies = c('import'))
 
Download the cedargraph code (from gitlab, or from tar.gz) and unzip anywhere

In R or Rstudio, open the folder containing the package.  To do so in R studio, double click or open 
'cedargraph.Rproj'

Once open, first install this projects dependencies while in R

     > devtools::install__deps('.')

Now you may using the Build tools to build the open package.  In RStudio, use the "build menu and select build and 


    > devtools::install()
    > library(cedargraph)

Test cedargraph by opening inst/examples/app.R in Rstudio

    > shiny::runApp('inst/examples')

Note that on Windows RStudio, when you run the example Shiny app, it may open
in an RStudio web viewer window.  Unfortunately, the graph will not display in this window
It will display if you open the app in a browser other than Internet Explorer 11

To install CEDAR

download zip or tar.gz file and extract all files to anywhere
OPen the project up in Rstudio by opening cedar.Rproj
Install the dependencies using 

    > devtools::install_deps(.)
    > devtools::install()
    > library(cedar)
    > shiny::runApp('inst/examples') 



