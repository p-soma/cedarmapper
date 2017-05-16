#' mappertest.R
# TODO re-write this to use library(testthat) http://r-pkgs.had.co.nz/tests.html

#' visual test of mapper class should plot a connect graph of 6 nodes in hexagon shape
#' return a table of the variance for each variable for each group
#' @export
circle.mapper<- function(npoints=100, randomize=FALSE) {
  projection_lense  <- function(var="X"){
    lense(lense.projection, lenseparam=var, partition_count=6, overlap = 0.5)
  }
  
  gm= mapper(circle_data(1, npoints,randomize=randomize),
             lenses=list(projection_lense("X"),projection_lense("Y")),
             bin_count=10, 
             normalize_data = FALSE)
  
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

#'grid mapper test
#' uses run.mapper to create mapper of uniform grid data and plot it
#'@export
grid.mapper.test <- function(npoints = 10){
  gm <- grid.mapper() %>% mapper.run
  print ( gm$partitions)
  plot(graph.mapper(gm), 
       main = paste0("grid data, ", nrow(gm$d), " points per dimension"),
       sub=paste0(gm$lenses[[1]]$n, " partitions ", gm$lenses[[1]]$o," overlap")
       )
  return( (gm))
}

#TODO 
#TODO multivariate test

