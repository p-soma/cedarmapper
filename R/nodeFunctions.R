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
makegraphmapper <- function(x, lensefun, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap", lenseparam = NULL){
  gm = structure(list(d = x, 
                      "partition_count"=partition_count, 
                      "overlap" = overlap,   # percent, o <= 1
                      "lensefun"=lensefun, 
                      "partition_method"=partition_method, 
                      "index_method"=index_method), 
                      "lenseparam" = lenseparam,
                 class="graphmapper")
  
  
  gm$partitions = partition.graphmapper(gm)
  
  # list of clusters using euclidean distance, single linkage, and  gap clustering detection, 
  gm[["clusters"]]   = clusters.graphmapper(gm,200)
  
  # from clusters create nodes of sets of d
  gm[["nodes"]]     = nodes.graphmapper(gm)
  
  gm[["adjmatrix"]] = adjacency.graphmapper(gm) 

  return(gm)
  
}

#' @export
partition <- function(d, lensefun, n=4, o=0.5, lenseparam=NULL){
 
  # calculate lense values for all rows in d, use optional parameter
  if(is.null(lenseparam)) {
    lense.df = lensefun(d)  
  } else {
    lense.df = lensefun(d,lenseparam)
  }
  
  # returns data frame with value L and rowid ID
  
  # partition length = linear distance

  total_length = max(lense.df$L) - min(lense.df$L)
  pl = total_length/(n - ((n-1)*o))
  p0 = min(lense.df$L)
  
  partitions = list()
  
  # TODO: rewrite to use 'ldapply'  instead of forloop for when n > 10^6
  for (i in 1:n) {
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
  gm[["partitions"]]  = partition(gm$d, gm$lensefun, gm$partition_count,  gm$overlap, gm[["lenseparam"]])
} 


# cluster detection using NbClust; 
# the clustering built in so works on partitions
#' @export
clusters.graphmapper<- function(gm,iterations=500) {
  distance_method = "euclidean"
  index_method    = "single"
  distanceFunction <- function(x) dist(x, method=distance_method)
  clusterFunction  <- function(x, k) list(cluster=cutree(hclust(dist(x), method = "single"),k=k))
  gapFunction      <- function(x){ 
    maxClusters = sqrt(nrow(x))*2
    gf =clusGap(x, FUNcluster = clusterFunction, K.max = maxClusters, B = iterations)
    kvalue = with(gf,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
    return(clusterFunction(x,kvalue)$cluster)
    }   
  
  clusterResult <- function(x, k) list(cluster=cutree(hclust(mydist(x), method = "average"),k=k))

  gmClusts = list()
  # TODO: use ldapply instead of for loop
  for ( i in 1:length(gm$partitions)) {
    # if (debug) print(i)
    # subset of data for this partition, which contains row names...
    # this is not necessary here, and adds to R memory burden for large partitions
    # but adds to code readability
    # gf = gapFunction(p)
    print("analyzing partition")
    x = as.matrix(gm$d[gm$partitions[[i]],])
    #nb = NbClust( x, 
    #             distance = "euclidean", 
    #             method=    "single",
    #             min.nc = 1, max.nc = sqrt(nrow(x))*2,              #TO DO  nrow(x)/2,
    #             index = "gap"  )  # gm$index_method
    #gmClusts[[i]] =  nb$Best.partition
    gmClusts[[i]] = gapFunction(x)
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
  names(nodes)<- 1:length(nodes)
  return(nodes)
}


#' @export
adjacency.graphmapper<- function(gm) {
  # shorten the name
  nodes = gm$nodes
  # function that tells if there is overlapping rows of data
  detect_overlap <- function(a,b) { length(intersect(nodes[[a]], nodes[[b]])) }
  
  adjmat <- matrix(0, ncol = length(nodes), nrow = length(nodes))
  colnames(adjmat) <- rownames(adjmat) <- names(nodes)
  for(i in 1:nrow(adjmat)) {
    for(j in 1:ncol(adjmat)) {
      if (i == j) {
        adjmat[i,j] = 0
      } else {adjmat[i,j] <- detect_overlap(i,j)}
    }
  }
  
  # return only upper matrix (undirected graph)
  adjmat[lower.tri(adjmat)] <- 0
  return(adjmat)
}

#  uses igraph 
#' @export
graph.graphmapper<- function(gm){
  # if no adj matrix, get one
  if (is.null(gm[["adjmatrix"]])) gm[["adjmatrix"]] = adjacency.graphmapper(gm)
  # need to use just upper half of matrix
  g  <- graph_from_adjacency_matrix(gm$adjmatrix, mode ="undirected",weighted="weight", diag=FALSE)
  return(g)
}


####### helpers
#' @export
nodedata <- function(nodeid, gm){
  # ensure gm is a graphmapper
  # ensure gm has 
  return(gm$d[gm$nodes[[nodeid]],])
}

#' @export
nodelistdata <- function(node_ids, gm){
  # nodelist is a vector of nod
  gm$d[unlist(gm$nodes[node_ids]),]
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

# plotting 

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

