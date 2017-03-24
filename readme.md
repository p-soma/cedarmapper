CEDAR project
===

This is an R library and Shiny app for the MSU CEDAR project of Dr. Jose Perea.  


Installation  (DRAFT)
---

***Prerequisites***

The CEDAR mapper pipeline is implemented in R, and the CEDAR user interface is implented using the Shiny application framework from Rstudio https://shiny.rstudio.com/ 

Download and Install R, select the latest version (>= 3.3.1)

Windows users may also need to download and install Rtools (version 3.4 ) from https://cran.rstudio.com/bin/windows/Rtools/    Note: when installing don't need to the TCL/TK extras for these tools for CEDAR. 

Install Rstudio latest version ( tested with 1.0.1x)
Install an alternative browser such as Chrome or Firefox.  Unfortunately the graph visualization does not currently work in the Rstudio internal browser or Internet Explorer (does not work in 7-10 or Edge)

***Installation in R***

In Rstudio, install 'devtools'  and include its dependencies with the following command:

> install.packages('devtools', dependencies = c('import'))
 
 
Once devtools is installed, you can install the Cedar package from github within Rstudio

devtools::install_github("MSU-CEDAR/cedarmapper",dependencies=T)

Note that on Windows RStudio, when you run the example Shiny app, it may open
in an RStudio web viewer window or Internet Explorer.  Unfortunately, the graph will not display in these windows/browsers.  Please consider using Google Chrome or Firefox.  Add support for other browsers is in progress. 
   
***Use***

Once the cedar is installed, you can start the application as follows

cedar::runCedar()

Which should launch in your default browser.   You can upload datasets within the Cedar application. 


Development
---

If you are invited to work on this project, please clone this repository to your computer, then use a command like  (replacing the folder here with your local folder)

    devtools::install_local('/Users/IEUser/Documents/cedarmapper', dependencies = T)

OR 

Open the cedar.Rproj file in the main cedarmapper directory, which will open the project in Rstudio, then use   

    devtools::build()
    library(cedar)

    or the Rstudio build menu. 
    

There are tests of cedar and the D3 HTML widget in the inst/examples/app.R, and can be run in Rstudio

    > library(cedar)
    > shiny::runApp('inst/examples')

    
To run examples in R, open R/examples folder.   Graphs are displayed using iGraph. 

To run the Cedar shiny application use the command  

      cedar::runCedar()      
      

Data
----

The dataset 'chemdiab' is from Reaven, G. M. and Miller, R. G. (1979). An attempt to define the nature of chemical diabetes using a multidimensional analysis. Diabetologia 16, 17-24,  This Rdata file was taken from the  'locfit' package https://cran.r-project.org/web/packages/locfit/ and put here for convenience.   

