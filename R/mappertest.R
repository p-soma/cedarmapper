#' mappertest.R
# TODO re-write this to use library(testthat) http://r-pkgs.had.co.nz/tests.html

#' visual test of mapper class should plot a connect graph of 6 nodes in hexagon shape
#' return a table of the variance for each variable for each group
#' @export
circle.mapper<- function(npoints=100, randomize=FALSE) {
  gm= mapper(circle_data(1, npoints,randomize=randomize), lensefun=lense.projection, lenseparam="X", 
             partition_count=6, overlap = 0.5,bin_count=10, normalize_data = TRUE)
  gm$distance   <- distance.mapper(gm)
  gm$partitions <- partition.mapper(gm)
  print ( gm$partitions)
  gm$clusters   <- clusters.mapper(gm, cluster_method = "single", shinyProgressFunction=NULL ) 
  gm$nodes      <- nodes.mapper(gm)
  
  #TODO write as a unit test expecting 6 nodes, 6 edges each node has degree 2
  gm$adjmatrix  <- adjacency.mapper(gm) 
  
  plot(graph.mapper(gm), main = paste0("Circle data, ", nrow(gm$d), " points"))
  
  return(gm)
}

#TODO 
#TODO multivariate test

