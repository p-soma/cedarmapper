CEDAR project
------

This is a  Shiny app for the MSU CEDAR project of Dr. Jose Perea. 

It employs the concurrent HTMLWidget visualization project [CedarGraph](https://gitlab.msu.edu/billspat/cedargraph) which also must be installed following the instructions here. 


Installation  (DRAFT)
----
Developing CEDAR on Windows

Notes
Installing the pieces necesarry to 'develop' a package requires more manual steps that simply installing a package from the R network CRAN.   When one installs a package from CRAN, all the dependencies are automatically installed.   Not so for development packages so these instructions are more complex.    


Download and Install R, select the latest version (3.3.1 as of this writing)

Windows users need to download and install Rtools (version 3.4 ) from https://cran.rstudio.com/bin/windows/Rtools/    Note: when installing don't need to the TCL/TK extras for these tools for CEDAR. 

Download and Install Rstudio latest version (no longer requires the preview version)

Start R or Rstudio

install 'devtools'  and include its dependencies with the following command:

    > install.packages('devtools', dependencies = c('import'))
 
Download the cedargraph code (from gitlab, or from tar.gz) and unzip anywhere

In R or Rstudio, open the folder containing the package.  To do so in R studio, double click or open  'cedargraph.Rproj'

Once open, first install this projects dependencies while in R

     > devtools::install_deps()

Now you may using the Build tools to build the open package.  In RStudio, use the "build menu and select build and 


    > devtools::install()
    > library(cedargraph)

Test cedargraph by opening inst/examples/app.R in Rstudio

    > shiny::runApp('inst/examples')

During development some required libraries may not have been ommitted from the DESCRIPTION file and R will ask you to install them manually.  

Note that on Windows RStudio, when you run the example Shiny app, it may open
in an RStudio web viewer window.  Unfortunately, the graph will not display in this window
It will display if you open the app in a browser other than Internet Explorer 11

To install CEDAR

download zip or tar.gz file and extract all files to anywhere
OPen the project up in Rstudio by opening cedar.Rproj
Install the dependencies using 

    > devtools::install_deps()
    > devtools::install()
    
During development some required libraries may not have been ommitted from the DESCRIPTION file.  You may have errors during install, loading the library or running the app.  Unfortunately you'll have to install them manually with `install.packages()`, or edit the DESCRIPTION file and re-run `devtools::install_deps()`

    > library(cedar)
    > shiny::runApp('inst/examples') 

   
Use
---

Open the Cedar project folder in Rstudio, or open the cedar.Rproj file which will open the project in Rstudio.    

To run examples in R, open examples folder and run our source the examples.   Graphs are displayed using iGraph. 

To run the Cedar shiny application, and either use command 

   shiny::runApp('inst/cedar')
  
or open one of the following files that are in the inst/cedar folder: global.R, server.R or ui.R, then when opened, click the "run app" green triangle button in Rstudio.  Requires Rstudio to run.  
Note that the CedarGraph D3 javascript visualization code doesn't work in the internal Rstudio browswer.  The app will run but the graph won't display.   If you open in external browser (any of Firefox, Safari, or Chrome) the graph should display.   

A Common source of problems is that the required libraries that have not been installed in your Rstudio.  If you get an error that a library is required, you may have to install it manually (using the pacakges feature of Rstudio).



Data
----

Appropriate data is from Reaven, G. M. and Miller, R. G. (1979). An attempt to define the nature of chemical diabetes using a multidimensional analysis. Diabetologia 16, 17-24,  This Rdata file was taken from the  'locfit' package https://cran.r-project.org/web/packages/locfit/ and put here for convenience.   

