# graph widget demonstration and support functions
###### 
# TODO: replace "name" attribute of nodes as "nodeid" here AND In all javascript
# source("R/cedarFunctions.R")

#'@import cluster
#'@import NbClust
#'@import igraph
#'@import htmlwidgets

#' @export
selectedrows <- function(nodelist) {ldply(nodes[nodelist], data.frame)}


#' @export
graphPrep <- function(nodes){
  adjmatrix = cedar.adj(nodes)
  # create an edge list
  return(cedar.graph(adjmatrix))
}

#'@export
nodePrep = function(nodes, selectedVariable=""){
  ## get node data ready for js widget
  
  #### prep nodes -- currently this only works with data with X variable
  # TO DO: introspect columns in nodes and add means for all columns
  # selectedVariable should be string of variable name, e.g. "X"
  meanVariable <- function(litem) { mean(litem$X)}
  g = graphPrep(nodes)
  # nodegraph javascript is expecting a "name" attribute which is the nodeID
  nodes_prepped = data.frame("name"  = as.vector((V(g))), 
                     "values"= unlist(lapply(nodes,meanVariable)))
  return(nodes_prepped)
}


linkPrep <- function(nodes){
  ## get link data for js widget

  # create a graph object
  g = graphPrep(nodes)
  links = get.data.frame(g)
  # make it work for javascript zero-based indexing
  links_prepped = data.frame(source=links$from - 1, target = links$to - 1 , weight = links$weight)
  
  return(links_prepped)

}

saveGraphData <- function(nodes, filename="cedardata.rda"){
  datalinks = linkPrep(nodes)
  datanodes = nodePrep(nodes)
  save(datalinks, datanodes, file=paste("data",filename,sep="/"))
}

# exampe of how to display widget
# nodes = circlenodes(100)
# cedarGraph(linkPrep(nodes), nodePrep(nodes))

