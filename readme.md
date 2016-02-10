CEDAR project
------

This is a  Shiny app for the MSU CEDAR project of Dr. Jose Perea. 

It employs the concurrent HTMLWidget visualization project [CedarGraph](https://gitlab.msu.edu/billspat/cedargraph).  

Currently to use this HTMLWidget in your code, you have to clone the repo to your hard disk, then use the following

      library(devtools)
      install_git("/local/path/to/cedargraph")
      
Installation from gitlab.msu.edu currently doesn't work well, especially on windows

Data
----

Appropriate data is from Reaven, G. M. and Miller, R. G. (1979). An attempt to define the nature of chemical diabetes using a multidimensional analysis. Diabetologia 16, 17-24, which is currently available in the 'locfit' package.  

   install.packages('locfit')
   library(locfit)
   data(chemdiab)

No other functions from the 'locfit' package are used. 

