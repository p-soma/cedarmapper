# cedarFunctions.R
# cedar project development functions

library(cluster)
library(NbClust)
library(igraph)

# for gap and colored dendograms, optional
library(factoextra)
# to install the above requires 'Hmisc' which requires binary install on Mac
# see https://cran.r-project.org/web/packages/Hmisc/index.html 

# generate a set of points on circle
# example c = randCircle(); plot(c)

main <- function(npoints = 100,make_plots=FALSE) {
  
  # d is for data
  d = circle.data(r=1,n=npoints, randomize=FALSE)
  
  # lense partitions
  d.partitions= cedar.partition(d, l = 4)
  
  # list of clusters using euclidean distance, single linkage, and  gap clustering detection, 
  d.clusters  = cedar.clusters(d, d.partitions)
  
  # from clusters create nodes of sets of d
  d.nodes     = cedar.nodes(d,d.clusters)
  
  # look for links and build adjacency_matrix
  d.adjmatrix = cedar.adj(d.nodes)
  
  # create an edge list
  d.graph     = cedar.graph(d.adjmatrix) 
  
  if(make_plots) {
    
    par(mfrow=c(2,2))
    
    plot(d$X, d$Y, main="Unit Circle")
    par(mfrow=c(2,2)) # set for 2X2 plot
    for(i in 1:4){ with(d.partitions[[i]], plot(X,Y)) }
    par(mfrow=c(1,1))
    
    par(mfrow=c(2,2))
    for(i in 1:4){ 
      d.subset = d.partitions[[i]][,-3]  # remove the ID column,TODO remove hard coded col num
      print( eclust(d.subset, FUNcluster="hclust", k.max = 5, stand =TRUE, B = 500, hc_metric="euclidean", hc_method="single"))
    }
    
    par(mfrow=c(1,1))
    plot(d.graph, main="resulting graph")
    
  
  }
  
  # circle_cluster_gap_viz(d,d.partitions)
  
  return(d.nodes)
  
}


######DATA
circle.data <- function(r=1, n=60, randomize=FALSE) {
  if (randomize){
    angles = (runif(n, -1*r, 1*r)) * pi
  } else {
    # evenly spaced arcs
    angles = (0:(n-1))*2*(pi/n)    
  }
  
  d = data.frame(X=cos(angles)*r, Y=sin(angles)*r)
  # add an ID number
  d = data.frame(d,ID=seq.int(n))
  rownames(d) = d$ID
  return(d)
}

# partitioning using the Y column only, 4 groups essentially built in, 
# even though there is a parameter here it's not really used. 
cedar.partition <- function(d, l = 4) {
  
  # our simple lense function
  circle.lense = function(d) {
    lense_data = data.frame(L=d$Y, ID=seq.int(nrow(d)))
    return(lense_data)
  }
  
  # lense function here returns data frame with column L
  # could be a simple array or vector at this point with named rows
  lense.df = circle.lense(d) 
  
  # simple partitioning paritions with 50% overlap; 
  partition_size = 2 * ( max(lense.df$L) - min(lense.df$L))/(l+1)  # l = num of partitions
  
  # ? sort result from lense function
  # lense.df = lense.df[with(lense.df, order(L)), ]
  partitions = list()
  lenses = list()
  for (i in 1:l) {
    offset= partition_size * (i - 1) * 0.5  # offset is 1/2 partition size X parttion number
    lenses[[i]] = lense.df[(lense.df$L >= (min(lense.df$L) + offset)) & (lense.df$L <= (min(lense.df$L) + partition_size+offset)),]
    partitions[[i]] = d[lenses[[i]]$ID,]
    
  }
  # to test that all rows are present
  # nrow(d) == length(sort(unique(c(lense.list[[1]]$ID, lense.list[[2]]$ID,lense.list[[4]]$ID, lense.list[[3]]$ID))))
  return(partitions)
} 


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
# cluster detection using NbClust; 
# the clustering built in so works on partitions
cedar.clusters<- function(d,partitions) {
  l = length(partitions)
  nbClusts = list()
  
  for ( i in 1:l) {
    d.subset = partitions[[i]][,-3]  # remove the ID column,TODO remove hard coded col num
    nb = NbClust(as.matrix(d.subset), distance = "euclidean", method="single",
            min.nc = 1, max.nc = 5,index = "gap")
    nbClusts[[i]] = nb$Best.partition
  }
  return(nbClusts)
}


cedar.nodes<- function(d,clusters){
  l = length(clusters)
  nodes = list()
  node_counter = 0
  
  # for each partition clustering
  for ( i in 1:l) {
    
    # shorten the name the item from the clusters list
    cuts = clusters[[i]] 
    
    # cuts are a list of data points with associated cluster numbers, 1,1,2,2,...,k
    # convert this repeating list of cluster numbers to just the numbers 1,2,..k
    for(j in unique(cuts)){
      # advance the node counter TODO: vectorize this loop
      node_counter = node_counter + 1
      data_subset_for_this_cluster = d[names(cuts[cuts == j]), ]  
      # store these data inside this node
      nodes[[node_counter]] = data_subset_for_this_cluster
    }
  }
  return(nodes)
}


cedar.adj<- function(nodes) {
  # function that tells if there is overlapping rows of data
  detect_overlap <- function(a,b) { length(intersect(a$ID,b$ID)) }
  
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


cedar.graph<- function(adjmatrix){
  # need to use just upper half of matrix
  adjmatrix[lower.tri(adjmatrix)] <- 0
  
  g  <- graph_from_adjacency_matrix(adjmatrix, mode ="undirected",weighted="weight", diag=FALSE)

  return(g)
}

