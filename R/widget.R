# graph widget demonstration and support functions

source("R/cedarFunctions.R")
#'@import cluster
#'@import NbClust
#'@import igraph
#'@import htmlwidgets

selectedrows <- function(nodelist) {ldply(nodes[nodelist], data.frame)}


#' @export
graphPrep <- function(nodes){
  adjmatrix = cedar.adj(nodes)
  # create an edge list
  return(cedar.graph(adjmatrix))
}

#'@export
nodePrep = function(nodes){
  ## get node data ready for js widget
  
  #### prep nodes -- currently this only works with circle data
  # TO DO: introspect columns in nodes and add means for all columns
  selectedVariable = "X"
  meanx <- function(litem) { mean(litem$X)}
  g = graphPrep(nodes)
  nodes.prepped = data.frame("name"  = as.vector((V(g))), 
                     "values"= unlist(lapply(nodes,meanx)))
  return(nodes.prepped)
}


linkPrep <- function(nodes){
  ## get link data for js widget
  # requires cedar functions to work
  
  adjmatrix = cedar.adj(nodes)
  # create an edge list
  g = cedar.graph(adjmatrix)
  links = get.data.frame(g)
  # make it work for javascript zero-based indexing
  links = data.frame(source=links$from - 1, target = links$to - 1 , weight = links$weight)
  
  return(links)

}

saveNodes <- function(nodes, filename="cedarcircle.rda"){
  circle.links = linkPrep(nodes)
  circle.nodes = nodePrep(nodes)
  save(circle.links, circle.nodes, file=paste("data",filename,sep="/"))
}

# note this doesn't seem to work unless run from console
nodes = main(100)
cedarGraph(linkPrep(nodes), nodePrep(nodes))

