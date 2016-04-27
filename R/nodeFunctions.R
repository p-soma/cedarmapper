# cedarFunctions.R
# cedar project development functions
#' @import cluster
#' @import NbClust
#' @import igraph

library(igraph)
library(NbClust)
library(cluster)

# for gap and colored dendograms, optional
library(factoextra)
# to install the above requires 'Hmisc' which requires binary install on Mac
# see https://cran.r-project.org/web/packages/Hmisc/index.html 


# graphmapper class factory
# this prepares the data structure, then runs each mapper step
#' @export
makegraphmapper <- function(x, lensefun, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap"){
  gm = structure(list(d = x, 
                      "partition_count"=partition_count, 
                      "overlap" = overlap,   # percent, o <= 1
                      "lensefun"=lensefun, 
                      "partition_method"=partition_method, 
                      "index_method"=index_method), 
                 class="graphmapper")
  
  
  gm$partitions = partition.graphmapper(gm)
  
  # list of clusters using euclidean distance, single linkage, and  gap clustering detection, 
  gm[["clusters"]]   = clusters.graphmapper(gm)
  
  # from clusters create nodes of sets of d
  gm[["nodes"]]     = nodes.graphmapper(gm)
  
  gm[["adjmatrix"]] = adjacency.graphmapper(gm) 
  return(gm)
  
}

#' @export
partition <- function(d, lensefun, l=4, o=0.5){
 
  # calculate lense values for all rows
  lense.df = lensefun(d)  # for now, don't allow extra parameters, but require them to be built into lense function 
  # returns data frame with value L and rowid ID
  
  # partition length = linear distance
  # for l=4, o = 0, partition length  = 40% of total length
  total_length = max(lense.df$L) - min(lense.df$L)
  pl = total_length * 0.4
  p0 = min(lense.df$L)
  
  partitions = list()
  
  # TODO: rewrite to use 'ldapply'  instead of forloop for when n > 10^6
  for (i in 1:l) {
    partition_start = p0 + pl * (i - 1) * (1-o)  # offset== starting value is 1/2 partition size X parttion number
    partition_end   = partition_start + pl
    partitions[[i]] = with(lense.df, lense.df[L >=  partition_start & L <= partition_end, "ID"])
    # previously was partition of data: partitions[[i]] = d[lenses[[i]]$ID,]; now just an ID
  }
  # to test that all rows have been included in at least one partition
  # if(nrow(d) != length(unique(unlist(partitions)))) stop("partitioning does not include all rows")
  return(partitions)
  
}

# creates a list of 'partitions' of dataset, using a reducing 'lense' function
#' @export
partition.graphmapper <- function(gm) {
  if (class(gm) != "graphmapper") stop("partition: requires input of class graphmapper class")
  gm[["partitions"]]  = partition(gm$d, gm$lensefun, gm$partition_count,  gm$overlap)
} 


# cluster detection using NbClust; 
# the clustering built in so works on partitions
#' @export
clusters.graphmapper<- function(gm,iterations=500) {
  distance_method = "euclidean"
  index_method    = "single"
  distanceFunction <- function(x) dist(x, method=distance_method)
  
  clusterFunction  <- function(x, k) list(cluster=cutree(hclust(dist(x), method = "single"),k=k))
  gapFunction      <- function(rowids){ clusGap(x=gm$d[rowids,], FUNcluster = clusterFunction, K.max = length(rowids)/10, B = iterations)}
 

  gmClusts = list()
  # TODO: use ldapply instead of for loop
  for ( i in 1:length(gm$partitions)) {
    # if (debug) print(i)
    # subset of data for this partition, which contains row names...
    # this is not necessary here, and adds to R memory burden for large partitions
    # but adds to code readability
    # gf = gapFunction(p)
    # numclusters = with(gf,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
    print("analyzing partition")
    x = as.matrix(gm$d[gm$partitions[[i]],])
    nb = NbClust( x, 
                 distance = "euclidean", 
                 method=    "single",
                 min.nc = 1, max.nc =    20,    # length(p)/2,
                 index = "gap"  )  # gm$index_method
    # R idiom for add to end of a list
    gmClusts[[i]] =  nb$Best.partition
    
    # gmClusts = list(gmClusts, list( nb$Best.partition ))
  }
  
  return(gmClusts)
}



# given a graph mapper object, create the nodes using the current clustered partitions
# nodes are subsets of IDs (rownames) based on optimal partitions
#' @export
nodes.graphmapper <- function(gm){
  l = length(gm$clusters)
  nodes = list()
  node_counter = 0
  
  # for each partition
  for ( i in 1:l) {
    
    # shortened name for the cluster list
    cuts = gm$clusters[[i]] 
    
    # cuts are a list of data points with associated cluster numbers, 1,1,2,2,...,k
    # convert this repeating list of cluster numbers to just the numbers 1,2,..k
    for(j in unique(cuts)){
      # advance the node counter TODO: vectorize this loop
      node_counter = node_counter + 1
      nodes[[node_counter]] = names(cuts[cuts == j])
      
    }
  }
  return(nodes)
}

#' @export
adjacency.graphmapper<- function(gm) {
  # shorten the name
  nodes = gm$nodes
  # function that tells if there is overlapping rows of data
  detect_overlap <- function(a,b) { length(intersect(a,b)) }
  
  adjmat <- matrix(0, ncol = length(nodes), nrow = length(nodes))
  colnames(adjmat) <- rownames(adjmat) <- names(nodes)
  for(i in 1:nrow(adjmat)) {
    for(j in 1:ncol(adjmat)) {
      if (i == j) {
        adjmat[i,j] = 0
      } else {adjmat[i,j] <- detect_overlap(nodes[[i]], nodes[[j]])}
    }
  }
  return(adjmat)
}

#' @export
graph.graphmapper<- function(gm){
  # need to use just upper half of matrix
  adjmatrix = cedar.adj(gm)
  adjmatrix[lower.tri(adjmatrix)] <- 0
  g  <- graph_from_adjacency_matrix(adjmatrix, mode ="undirected",weighted="weight", diag=FALSE)
  return(g)
}

# TODO: rename this function to reflect it's data prep
#' @export
cedar.graphprep = function(gm, varName="X"){
  # converts an gm objects into structures with nodes and links
  
  # check that variable is in nodes
  nodes = gm$nodes
  # TODO: detect if adjmatrix is present, and if not, recreate
  adjmatrix = gm$adjmatrix # cedar.adj(nodes)
  

  meanVariable <- function(litem) { mean(get(varName,litem))}
  
  # currently convert to lists to graph and then graph to lists
  # TODO: cut out the middleman and prep from adjmatrix directly! 
  g = cedar.graph(adjmatrix)
  nodes_prepped = data.frame("name"  = as.vector((V(g))), 
                             "values"= unlist(lapply(nodes,meanVariable)))
  
  links = get.data.frame(g)
  links_prepped = data.frame(source=links$from - 1, target = links$to - 1 , weight = links$weight)
  
  return( list(graph_nodes = nodes_prepped, graph_links = links_prepped) )
  
}

graph.graphmapper = function(gm){
  adjmatrix = gm$a
  adjmatrix[lower.tri(adjmatrix)] <- 0
  g  <- graph_from_adjacency_matrix(adjmatrix, mode ="undirected",weighted="weight", diag=FALSE)
  return(g)
}

####### LENSES
#' @export
simple_lense = function(d,varname=NULL ){
  # simple lense function that returns a single variable
  # d = data as a data frame, varname  = name of variable as a string
  
  # if no variable name passed, use the first name
  # requires data d to have named columns
  if (is.null(varname)) { varname = names(d)[1] }
  
  # prep data frame for partitioning function with L column 
  lense_data = data.frame(L=get(varname, d), ID=rownames(d), stringsAsFactors = FALSE)
  return(lense_data)
  
}

# simple lense on X coordinate
#' @export
x_lense <- function(d){
  simple_lense(d, "X")
}
# simple lense on Y coordinate
#' @export
y_lense <- function(d){
  simple_lense(d, "X")
}

#######################
### plotting 

plot.graphmapper <- function(gm){
  # create an edge list
  adjmatrix = cedar.adj(gm)
  cedar.graph(adjmatrix) 
}

plot_partitions <- function(gm, xvar="X", yvar="Y")  {
  partitions = gm$partitions
  par(mfrow=c(2,2)) # set for 2X2 plot
  for(i in 1:length(partitions)){ with(partitions[[i]], plot(X,Y)) }
  par(mfrow=c(1,1))
  
  par(mfrow=c(2,2))
  for(p in partitions){ 
    # remove the ID column,TODO remove hard coded col num
     
    print( eclust(gm$d[p,], FUNcluster="hclust", k.max = 5, stand =TRUE, B = 500, hc_metric="euclidean", hc_method="single"))
  }
}    

plot_cluster = function(gm, cnumber){
  cldata = cbind(d.partitions[[cnumber]],gm$clusters[[cnumber]])
  plot(cldata$X, cldata$Y, col = c("red", "green", "blue")[cldata$`d.clusters[[cnumber]]`])
  # d[d$ID %in% names(cl[[1]]),]
}