library(TDAmapper)
library(fastcluster)
library(networkD3)
library(cedargraph)

# functions to work with simply circle data using the TDAmapper package
# creates list of nodes and links from circle data that can be used with NetworkD3
# TODO: apapt to work with cedargraph

# extract nodes and edges from mapper for use with cedar graph and eventually cedarshiny
# links have "source","target","weight"
# nodes have "name":1,"values":1.59733236811928e-16,"size":10

# Mapper object members:
  
  # adjacency  == graph structure
  # num_vertices == same as lenght/width of adjacnecy matrix
  # level_of_vertex  = the level set that contains vertices
  # points_in_vertex = in each v
  # points_in_level_set,  - level sets are collections of vertices
  # vertices_in_level_set = which vertices are in each levelset
  
  # currently the "name" field is the same as the "nodeid in cedar, which in TDAMapper is the vertex id
  # mapper doesn't store the original data set, only the filter values, but when making nodes can add 
  # need to make new function
  # 

#' DRAFT # given a tdamapper and the original dataset, create vertices for use with cedargraph function
cedarVertices <-function(tdamapper,dataset){
  # cedargraph expects nodes like: 
  # "nodes":[{"name":1,"values":1.59733236811928e-16,"size":10},
  # name is the vertex ID, values = mean of filter in the of the column to include
  # 
  
}

cedarEdges <-function(tdamapper, dataset){
  #   links":[{"source":0,"target":1,"weight":7},
  
  
}

# ' this function creates tdamapper object from circle data using single 
tdacircle <- function(npoints=100, coordname="Y") {
    c = circle_data(npoints)
    f = as.vector(c[[coordname]]) # lense or filter function  = single coordinate projection
    c.mapper = TDAmapper::mapper1D(distance_matrix = dist(c), 
       filter_values=f, 
       num_intervals=4, 
       percent_overlap = 50,
       num_bins_when_clustering = 10)

    pt_labels <- 1:length(f)
    nodes = mapperVertices(c.mapper,pt_labels )
    links = mapperEdges(c.mapper)
    
    return(list(Nodes=nodes, Links=links))
}


#forceNetwork(Nodes =nodes, Links = links, 
#             Source = "Linksource", Target = "Linktarget",
#             Value = "Linkvalue", NodeID = "Nodename",
#             Group = "Nodegroup", opacity = 0.8, 
#             linkDistance = 10, charge = -400)    

#gmtest<- function(npoints=100) {
#  makegraphmapper(circle_data(1, npoints), circle_lense, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap")
#}

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



