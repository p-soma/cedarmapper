# mapper.R
# This is the main mapper object and  pipeline 

# UNFINSIHED WORK IN PROGRESS lense object for n dimensions >= 1
# notes: don't set the number of dimensions, instead simply run each lense in the 
# list of lenses, so essentially dismensions= length(lenses)
#' @import cluster

#library(igraph)
library(cluster)

# SETUP GLOBAL VAR for debugging
# assign("DEBUG", FALSE,.GlobalEnv)

# for gap and colored dendograms, optional, uncomment to for printing dendograms
# library(factoextra)
# to install the above requires 'Hmisc' which requires binary install on Mac
# see https://cran.r-project.org/web/packages/Hmisc/index.html 


#' object structure for lenses (filters) to build dimensional cover, one lense per dimension
#' @param lensefun A function to reduce dimensionality of dataframe
#' @param partition_count A postive integer number of partitions to create
#' @param overlap The percent the partitions overlap as decimal value, default 0.5
#' @return lense object for a single dimension of the covering
#' @export
lense <- function(lensefun, lenseparam=NULL, partition_count=4, overlap = 0.5) {
  # note: using as.numeric to convert arguments becuase Shiny inputs return strings
  L <- structure( list("lensefun"  = lensefun, 
                       "lenseparam"= lenseparam,  # don't use as.numeric here, sometimes is variable name
                       "n"         = as.numeric(partition_count),  # or should be n
                       "o"         = as.numeric(overlap)),
                       "p0"        = NULL, # lower bound of lense values, filled in when L values calculated
                       "pl"  = NULL,  # partition length
                  class = "lense")

  # TODO validate partition_count > 0 integer
  # TODO validate overlap  between 1 and 0
  
  return(L)
}

#' Constructor for mapper object to be used in mapper pipeline
#' @return mapper object with all params needed for pipeline
#' @export
mapper <- function(dataset, lenses, cluster_method="single", bin_count=10, normalize_data=TRUE){
  # lenses have previous parameters used: lensefun, partition_count=4, overlap = 0.5,lenseparam = NULL
  
  # note: dimensions variable 
  # note: using as.numeric to convert arguments becuase Shiny inputs return strings
    
  #  "partition_count"=as.numeric(partition_count), 
  #  "overlap" = as.numeric(overlap),   # percent, o <= 1
   gm = structure(  list(d = dataset, 
                      "lenses"=lense, 
                      "cluster_method"=cluster_method, 
                      "bin_count" = as.numeric(bin_count),
                      "normalize_data" = normalize_data
                      ),
                 class="mapper")
  
  ### TODO ; remove all as.numeric conversions here as we need to allow lists
  ## put all conversions inside the Shiny app 
  
  rownames(dataset)<- 1:nrow(dataset)
    gm$distance   <- NULL
    gm$partitions <- NULL
    gm$clusters   <- NULL
    gm$nodes      <- NULL
    gm$adjmatrix  <- NULL
    gm$groups     <- list()
    
  return(gm)
}

#' return the dimension of a graph mapper object by counting lenses
#' @param gm graphmapper object
#' @return integer >1 or NA
#' @export
mapper.dimensions <-function(gm){
  length(gm$lenses)
}

# single method to run all steps for mapper pipeline given a mapper object
#' @export
mapper.run <- function(gm){
  
  gm$distance   <- distance.mapper(gm,method="euclidean") # dist(scale(gm$d),method="euclidean", upper=FALSE)
  gm$partitions  <- partition.mapper(gm)
  gm$clusters   <- clusters.mapper(gm, cluster_method = cluster_method, shinyProgressFunction=progressUpdater ) 
  gm$nodes      <- nodes.mapper(gm)
  gm$adjmatrix  <- adjacency.mapper(gm) 
  return(gm)

}

#  single method to collect parameters and then run all steps for 1D mapper object
#  
#' @export
makemapper <- function(dataset, lensefun, partition_count=4, overlap = 0.5,  
                       bin_count=10, cluster_method= 'single', lenseparam = NULL, 
                       normalize_data=TRUE, progressUpdater=NULL){
  # create objects with the above params
  one_lense <- lense(lensefun, lenseparam, partition_count, overlap)
  gm <- mapper(dataset=dataset, 
               lenses=list(one_lense),
               cluster_method=cluster_method, 
               bin_count=bin_count, 
               normalize_data=normalize_data)
  return(run.mapper(gm))
}

#' calculate distance matrix of the existing data, scale if normalize data is checked
#' @param gm A mapper object with d data member
#' @param method A string method name used by the dist() function
#' @return distance matrix of data element of mapper object gm$d
distance.mapper <- function(gm, method="euclidean") {
  # same method, just using scaled data or not
  if ( gm$normalize_data ) 
    { dist(scale(gm$d),method, upper=FALSE)  }
  else 
    { dist(gm$d,method, upper=FALSE) }

}
  

#"lensefun"  = lensefun, 
#"lenseparam"= lenseparam,  # don't use as.numeric here, sometimes is variable name
#"n"         = as.numeric(partition_count),  # or should be n
#"o"         = as.numeric(overlap)),
#"p0"        = NULL, # lower bound of lense values, filled in when L values calculated
#"pl"  = NULL,  # partition length

lense.calculate <- function(gm,dimension=1){
  L <- gm$lense[[dimension]]
  if (class(L) != "lense") stop ("partition function requires a lense object")
  L$values <- L$lensefun(gm$d, L$lenseparam, gm$distance)
  # L is 1D vector of values from the filter/lense function with same length as D
  # copy names of rows e.g. rowids to L
  names(L$values) <- rownames(gm$d)
  L$p0 <- min(L$values)
  total_length = max(L) - p0
  L$pl <- total_length/(n - ((n-1)*lense$o))
  return (L)
}

partition_start <- function(L,partition_index){
  if (class(L) != "lense") stop ("partition function requires a lense object")
  return( L$p0 + (L$pl * (partition_index - 1) * (1-L$o)) )
  # offset== starting value is 1/2 partition size X parttion number
}

get_partition_index <- function(L,this_partition_start_value){
  if (class(L) != "lense") stop ("partition function requires a lense object")
  # L must have been 'calculated' to load these values
  i = (  ((this_partition_start_value - L$p0) / L$pl) + (1-L$o)) / (1-L$o)    
}

partition_end <- function(L,i) {
  return( partition_start(L,i) + L$pl )
}



# given a value from a Lense, which partition is it in?
partition_index_for_l_value <- function(L,l_value){
  if (class(L) != "lense") stop ("partition function requires a lense object")
  # first use the index calculator on this l_value that's greater than the Partition start, so will have fractional part
  index_plus_something = get_partition_index(l_value)
  # the index is the nearest integer to this calculation
  partition_1_index = floor(index_plus_something)
  # given partitions overlap, does this Lense value fit in the lower partition?
  # if it's more than the overlap away from parition value, it's past end of previous partition
  distance_from_partition_start = index_plus_something - partition_1_index
  if ( distance_from_partition_start < lense$o & partition_1_index > 1 ) { 
    partition_2_index = partition_1_index - 1  
  } else {
    partition_2_index = NA
  }
  
  # return the 2 indexes for when o < 0.5
  # TODO add 3rd index when  o > .5 
  return( c(partition_1_index, partition_2_index))
  
}


#' partition a mapper object dataset by reducing dimensions via lense or filter function
#' lense function must be defined in the mapper object, and must return vector with 
#' rownames preserved
#' NOTES : lense objects now include all partitioning parameters (see above), and mapper object has list of lenses
#' NOTES : WIP use just the first lense in the list for 1D case
#'  par
#'  p
#' @param gm mapper object with dataset and lense function and lense parameters
#' @return list of vectors of rownames from the dataset, e.g. subsets or partitions 
#' @export
partition.mapper <- function(gm) {
  if (class(gm) != "mapper") stop("partition: requires input of class mapper class")
  # if (class(gm$lense[[1]]) != "lense") stop ("partition function requires a lense object")
  if (is.null(gm$distance)) { gm$distance = distance.mapper }  ## TODO this is not saved as pass by value which is inefficient
  
  gm$partitions = list()  # list of dimenions, and partitions inside each dimensionl

  # global gm mapper object present
  assign_partitions <- function(gm, dimension = 1) {
    # update list item in lense object with calculated values
    gm$lense[[dimension]] <- lense.calculate(gm,dimension)
    L = gm$lense[[dimension]]
    
    # n empty partitions to be filled with point IDs.  R idiom to create empty list n items
    # partitions is a list, each item is a vector of rowids of rows belonging to the partition
    partitions = vector("list", L$n)  
    for(i in 1:length(L$lvalues)){
      # get the partitions in which the value goes
      
      rowids = rownames(L$values[i])
      # for each overlapping partition that the value is part of...append values rowid to partition list 
      for(p in partition_index_for_l_value(L,L$values[i]) )
          partitions[[ p ]] = c(partitions[[ p ]],rowids) 
          }
    }
    
  
  }
  
    ## inefficient way to assign values to a parition
    # partition_values = function(i){
    #  partition_start = p0 + (pl * (i - 1) * (1-o))  # offset== starting value is 1/2 partition size X parttion number
    #  partition_end   = partition_start + pl
    #
    #  return(L[L >=  partition_start & L < partition_end ])
    # }
    # partitions = lapply(1:n,partition_values)
  
  # Note : here is a test that all rows have been included in at least one partition
  # if(nrow(gm$d) != length(unique(unlist(partitions)))) stop("partitioning does not include all rows")
 
  ## test function used to remove empty partitions
  non_empty = function(x) { length(x)>0}
  
  # return list of partitions,removing empty ones
  return(partitions[sapply(partitions, non_empty)])

} 


#' Mapper clustering of each partition using histogram method
#' this function creates a list with same length as partitions, 
#' each list item containing vector, with same labels/names as rowids,  of cluster ids from cutree 
#' function
# eg. for partition[[1]];  gmclusts[[1]]  = 
# 114 115 116 117 126 127 133 134 143 145 
# 2   1   2   2   2   2   2   1   2   3
#' @param gm mapper object
#' @param distance_method how is the distance matrix computed, default euclidean
#' @param index_method clustering method default single linkage 
#' @param scaling scale the data prior to calculating the distance matrix True/False
#' @return list for each partition of vectors as returned from cutree, cluster groups with names for dataset rows
#' eg. for partition[[1]];  gmclusts[[1]]  = 
#' 114 115 116 117 126 127 133 134 143 145 
#' 2   1   2   2   2   2   2   1   2   3
#' to be used by mapper node creation function
#' @export
clusters.mapper<- function(gm, cluster_method = "single", scaling=FALSE, shinyProgressFunction = NULL) {
  
  # TODO: remove cluster_method param and use gm object variable; ensure gm object has this set...

  gmClusts = list() 
  npartition = length(gm$partitions)
  # loop through each partition
  for ( i in 1:npartition) {
    print(paste0("analyzing partition ", i))
    
    # check for special case of only one datapoint, so no clustering necessary, break out of loop
    if(length(gm$partitions[[i]]) < 2 ){
      gmClusts[[i]] = c(1)
      names(gmClusts[[i]]) = rownames(gm$partitions[[i]])
      next
    }
    
    # SHINY STUFF for displaying progress bar when this is run; remove when parallelizing
    # If we were passed a shiny progress update function, call update each iteration
    if (is.function(shinyProgressFunction)) {
      text <- paste0("clustering partition ", i)
      shinyProgressFunction(value = (i/npartition), detail = text)
     }
    # end shiny stuff
    
    # debug 
    # print(gm$partitions[[i]])

    rowset = gm$d[names(gm$partitions[[i]]),] 
    
    # calculate distance matrix for this partition
    # TODO: check if whole data set partition is present, and extract subset from that
    partition_dist =  dist(rowset,method="euclidean")
    
    # if(DEBUG) {print(max(partition_dist))}
    # do standard clustering and cut 
    partition_cluster <- hclust(partition_dist, method=cluster_method) 
    cluster_cutheight <- cut_function(partition_cluster$height,  max( partition_dist), gm$bin_count)
    gmClusts[[i]] <- cutree(partition_cluster, h=cluster_cutheight )
    
    # note rowIDs are propagated in cluster$labels, so cutree returns groups labeled correctly
  }
  return(gmClusts)
}


clusters2d.mapper<- function(gm, cluster_method = "single", scaling=FALSE, shinyProgressFunction = NULL) {
  
  gmClusts = list() 
  npartition = length(gm$partitions)
  # loop through each partition
  for ( i in 1:npartition) {
    print(paste0("analyzing partition ", i))
    
    # check for special case of only one datapoint, so no clustering necessary, break out of loop
    if(length(gm$partitions[[i]][,1]) < 2 ){
      gmClusts[[i]] = c(1)
      names(gmClusts[[i]]) = rownames(gm$partitions[[i]])
      next
    }
    
    
    # SHINY STUFF for displaying progress bar when this is run; remove when parallelizing
    # If we were passed a shiny progress update function, call update each iteration
    if (is.function(shinyProgressFunction)) {
      text <- paste0("clustering partition ", i)
      shinyProgressFunction(value = (i/npartition), detail = text)
    }
    # end shiny stuff
    
    # debug 
    # print(gm$partitions[[i]])
    rowset = gm$d[rownames(gm$partitions[[i]]),] 
    
    # calculate distance matrix for this partition
    # TODO: check if whole data set partition is present, and extract subset from that
    partition_dist =  dist(rowset,method="euclidean")
    
    # if(DEBUG) {print(max(partition_dist))}
    # do standard clustering and cut 
    partition_cluster <- hclust(partition_dist, method="single") 
    cluster_cutheight <- cut_function(partition_cluster$height,  max( partition_dist), gm$bin_count)
    gmClusts[[i]] <- cutree(partition_cluster, h=cluster_cutheight )
    
    # note rowIDs are propagated in cluster$labels, so cutree returns groups labeled correctly
  }
  return(gmClusts)
}



#' histogram based cut function for single-linkage clusters
#' @export
#' @param cluster_heights heights of each cluster, height element from hclust output
#' @param maxdist maximum distance from data
#' @returns something 
cut_function  <-  function(cluster_heights, maxdist, bin_count) {
  # default cutoff is infinity, meaning 1 node
  cutoff <- Inf
  # if there  is one height value, then we have a single cluster
  if (length(cluster_heights) == 1) { 
    #if (cluster_heights == maxdist) {  # if this isn't true, then drop to code below 
    #    cutoff <- Inf
    #}
    return(cutoff)
  }
  
  minbin   = min(cluster_heights)
  maxbin   = maxdist
  binwidth = (maxbin - minbin)/bin_count
  bin_breaks <- seq(from=minbin,to=maxbin, by=binwidth)

  #print("cluster cut")
  #print(paste("seqparams=",minbin,maxbin,binwidth,sep=", "))
  #print(paste("hist of ",minbin,maxdist,sep=", "))
  #print(bin_breaks)
  
  height_hist <- hist(c(cluster_heights,maxdist), breaks=bin_breaks, plot=FALSE)
  
  z <- ( height_hist$counts == 0 )
  if (sum(z) != 0) {
    #  returns the indices of the logical vector (z == TRUE), min gives the smallest index
    cutoff <- height_hist$mids[ min(which(z == TRUE)) ]
  }
  return(cutoff)
}   

#' create the nodes from clustered partitions in mapper object
#' nodes are subsets of IDs (rownames) based on optimal partitions
#' @param gm graphampper object with paritions and clusters
#' @export
nodes.mapper <- function(gm){
  l = length(gm$clusters)
  nodes = list()
  node_counter = 0
  # TODO pre-allocatenode list as total number of nodes = sum(max(gm$clusters))
  # for each partition<-> cluster
  for ( i in 1:l) {

    # shortened name for the cluster list
    cuts = gm$clusters[[i]] 
    
    # cuts are a list of data points with associated cluster numbers, 1,1,2,2,...,k
    # convert this repeating list of cluster numbers to just the numbers 1,2,..k
    # TODO: which is faster unique or max(cuts) which assumes cuts are always natural sequence from 1
    for(j in unique(cuts)){
      # advance the node counter TODO: vectorize this loop
      node_counter = node_counter + 1
      # for each cut number, collect the rowids (names) into a vector and store in list item
      nodes[[node_counter]] = names(cuts[cuts == j])
    }
  }
  names(nodes)<- 1:length(nodes)
  return(nodes)
}


#' @export
adjacency.mapper<- function(gm) {
    # TODO: create an edge list instead and call it 
    # TODO: 
    # gm$edges <- findedges.mapper(gm)

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

# assign groups in gm object
#' @export
setgroup.mapper <-function(gm,node_ids,group_id = NULL) {
  
  if(!is.mapper(gm)) return(NULL)  # raise exception
  
  # setup default empty group list if not previously set
  if(!is.list(gm$groups)){ 
    gm$groups = list()
  } 
  
  g = gm$groups
  
  # default group id name is one more then current length
  if(is.null(group_id)) {
    group_id <- as.character(length(g) + 1)
  }
  
  g[[group_id]] <- union(g[[group_id]],as.numeric(node_ids))
  
  
  return(g)
}


