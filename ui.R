
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("column", "Choose a column:", 
                  choices = c("X",  "Y")),
      
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 30,
                  value = 10)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h3(textOutput("caption", container = span)),
      cedarGraphOutput("graph"),
      plotOutput("distPlot")
 
    )
  )
))
