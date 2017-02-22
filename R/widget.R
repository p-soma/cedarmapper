# graph widget demonstration and support functions
###### 
# exampe of how to display widget
# gm =   makegraphmapper(circle_data(1, 60), circle_lense, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap")
# cedarGraph(linkPrep(gm), nodePrep(gm))

# TODO: replace "name" attribute of nodes as "nodeid" here AND In all javascript
source("R/mapper.R")

#'@import htmlwidgets

#  may be obsolete...
# #' @export
# selectedrows <- function(nodelist) {ldply(nodes[nodelist], data.frame)}


# creates data frame of graph nodes suitable for conversion to JSON for cedargraph HTML widget
# includes internal functions for creating the 'value' of each node
#'@export
nodePrep = function(gm, selectedVariable=NULL){
  ## get node data ready for js widget

  # check that what's sent is a variable name
  # be nice and use a default variable name while developing Shiny
  selectedVariable = guaranteedVarname(gm, selectedVariable)
  
  meanVariable <- function(node) {
    mean(nodedata(gm, node,selectedVariable))
    # mean(gm$d[node,selectedVariable])
  }
  
  meanFilter <- function(node) {
    d = gm$d[node,]
    mean(gm$lensefun(d))
  }
  
  # g = graphPrep(nodes)
  # nodegraph javascript is expecting a "name" attribute which is the nodeID
  # the index of these nodes will be set when converted to json, 
  # and should be zero-based
  nodes_prepped = data.frame("name"  = as.numeric(names(gm$nodes)),    # this assumes node IDs are sequential; pls check 
                     "values"= unlist(lapply(gm$nodes,meanVariable)),
                     "size"  = unlist(lapply(gm$nodes,length))
                     )
  
  return(nodes_prepped)
}

# creates data frame of graph links suitable for conversion to JSON for cedargraph HTML widget
#' @export
linkPrep <- function(gm){
  ## get link data for js widget

  # create a graph object
  g = graph.mapper(gm)
  links = get.data.frame(g)
  # make it work for javascript zero-based indexing
  links_prepped = data.frame(source=as.numeric(links$from) - 1, target = as.numeric(links$to) - 1 , weight = links$weight)
  return(links_prepped)
}

# TODO: rename this function to reflect it's data prep
#' @export
graphPrep = function(gm, varName=NULL){
  
  # converts an gm object into structures with nodes and links for htmlwidget/shiny app
  # if using meanvar function, check that variable is in nodes
  return( list(graph_nodes = nodePrep(gm, varName), graph_links = linkPrep(gm)) )
  
}

