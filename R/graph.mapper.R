#' @import igraph
library(igraph)
#  uses igraph 
#' @export
graph.mapper<- function(gm){
  if(!is.mapper(gm)) return(NULL)  # TODO raise exception
  # if no adj matrix, get one
  if (is.null(gm[["adjmatrix"]])) gm[["adjmatrix"]] = adjacency.mapper(gm)
  # need to use just upper half of matrix
  g  <- graph_from_adjacency_matrix(gm$adjmatrix, mode ="undirected",weighted="weight", diag=FALSE)
  return(g)
}

