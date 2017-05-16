data("cedarcircle")

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
