
# examples..
# don't forget to run this when you start
# # after each code change

setupCedarGraph <- function () {
 library(htmlwidgets)
 library(devtools)
 devtools::install()
}


library(cedargraph)
# THIS DOESN'T SEEM TO WORK EITHER - gives us a promise and no data
# data(cedarcircle)

data("cedarcircle")

#data(MisLinks)
#data(MisNodes)


randomGraph <- function(n=20) {
  # Create graph data
  graph.data= randGraphData(n)
  cedarGraph(graph.data$links, graph.data$nodes,500,250)
}

circleSimpleGraph <- function() {
  # data(cedarcircle)  # circle.nodes, circle.links
  simpleGraph(circle.links, circle.nodes)
}

circleNodeGraph <- function() {
  # data(cedarcircle)  # circle.nodes, circle.links
  cedarGraph(circle.links, circle.nodes)
}


#cedargraph(Links = MisLinks, Nodes = MisNodes, Source = "source",
#             Target = "target", Value = "value", NodeID = "name",
#             Group = "group", opacity = 1, zoom = F, bounded = T,
#             clickAction = "alert('Ouch!')")
