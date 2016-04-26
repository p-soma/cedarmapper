
# functions to work with simply circle data
test<- function(npoints=100) {
  
  makegraphmapper(circle_data(1, npoints), circle_lense, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap")
  
}
# generate a set of points on circle
# example c = randCircle(); plot(c)
#' @export
circle_data <- function(r=1, n=60, randomize=FALSE) {
  if (randomize){
    angles = (runif(n, -1*r, 1*r)) * pi
  } else {
    # evenly spaced arcs
    angles = (0:(n-1))*2*(pi/n)    
  }
  
  d = data.frame(X=cos(angles)*r, Y=sin(angles)*r)
  # add an ID number - obsoleted; use rownames instead
  #d = data.frame(d,ID=seq.int(n))
  rownames(d) = 1:nrow(d)
  return(d)
}

# partitioning using the Y column only, 4 groups essentially built in, 
# even though there is a parameter here it's not really used. 



# our simple lense function
#' @export
circle_lense = function(d) {
  lense_data = data.frame(L=d$Y, ID=rownames(d), stringsAsFactors = FALSE)
  rownames(lense_data) = rownames(d)
  return(lense_data)
}

circlegraph <- function(r=1,n=npoints, randomize=FALSE){
  d = circle.data(r,n,randomize)  # data frame with X & Y values
  # gm  = graphmapper(d, ) etc
}

#' @export
circle_cluster_gap_viz <- function(d,partitions){
  l = length(partitions)
  o_par = par()
  par(mfrow = c(2,2))
  
  for ( i in 1:l) {
    # print dendograms with colored clusters
    
    d.subset = partitions[[i]][,-3]  # remove the ID column,TODO remove hard coded col num
    eClusts = eclust(d.subset, FUNcluster="hclust", k.max = 5, stand =TRUE, B = 500, hc_metric="euclidean", hc_method="single")
    # don't keep the value, just plot
  }
  
  for ( i in 1:l) {
    cGaps <- clusGap(as.matrix(d.subset), FUN = hcut, K.max = 5, B = 200, hc_metric="euclidean", hc_method="single")
    print(fviz_gap_stat(cGaps ))
  }
  par(o_par)
  
}

#' @export
circleapp <- function(){
  shiny::runApp(system.file('circleapp.R', package='cedar'))
}



