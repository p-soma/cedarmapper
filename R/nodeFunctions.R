# mapperFunctions.R
# This is the main mapper pipeline functions
# the main object is called graphmapper to avoid collision with other packages using the name 'mapper'

#' @import cluster
#' @import igraph

library(igraph)
library(cluster)

# SETUP GLOBAL VAR for debugging
# assign("DEBUG", FALSE,.GlobalEnv)

# for gap and colored dendograms, optional, uncomment to for printing dendograms
# library(factoextra)
# to install the above requires 'Hmisc' which requires binary install on Mac
# see https://cran.r-project.org/web/packages/Hmisc/index.html 

#  single method to run all steps for graphmapper object
#' @export
makegraphmapper <- function(dataset, lensefun, partition_count=4, overlap = 0.5, lensevals,  #coloring 
                            bin_count=10, cluster_method= 'single', lenseparam = NULL, 
                            normalize_data=TRUE, dimensions = 1, progressUpdater=NULL){
  # create object with the above params 

  
  gm <- graphmapper(dataset=dataset, 
                    lensefun=lensefun, 
                    partition_count=partition_count, 
                    overlap = overlap, 
                    lensevals = lensevals, # lensevals coloring
                    cluster_method=cluster_method, 
                    bin_count=bin_count, 
                    lenseparam=lenseparam,
                    normalize_data=normalize_data,
                    dimensions = dimensions)
  # notes:
  # for now, add the entire distance matrix to the object
  # the progressUpdater construct is for ShinyApps and optional
  gm$distance   <- distance.graphmapper(gm,method="euclidean") # dist(scale(gm$d),method="euclidean", upper=FALSE)
  gm$partitions <- partition.graphmapper(gm)
  gm$lensevals  <- data.frame(gm$lensefun(gm, gm$lenseparam))
  gm$clusters   <- clusters.graphmapper(gm, cluster_method = cluster_method, shinyProgressFunction=progressUpdater ) 
  gm$nodes      <- nodes.graphmapper(gm)
  gm$adjmatrix  <- adjacency.graphmapper(gm) 
  
  return(gm)
  
}


#' Constructor for graphmapper object to be used in mapper pipeline
#' @return graphmapper object with all params needed for pipeline
#' @export
graphmapper <- function(dataset, lensefun, partition_count=4, overlap = 0.5, lensevals=NULL,
                        cluster_method="single", bin_count=10, lenseparam = NULL,
                        normalize_data=TRUE, dimensions=1){
  # note: using as.numeric to convert arguments becuase Shiny inputs return strings
  ### TODO change default params to be lists to support n>1 dimension
  ## however, partition count should be a scalar, each partition having n dimensions
  
  gm = structure(  list(d = dataset, 
                      "partition_count"=as.numeric(partition_count), 
                      "overlap" = as.numeric(overlap),   # percent, o <= 1
                      "lensefun"=lensefun, 
                      "lensevals"=lensevals, #lense vals coloring
                      "cluster_method"=cluster_method, 
                      "lenseparam" = lenseparam,  # don't use as.numeric here, sometimes is variable name
                      "bin_count" = as.numeric(bin_count),
                      "normalize_data" = normalize_data,
                      "dimensions" = dimensions      ),
                 class="graphmapper")
  
  ### TODO ; remove all as.numeric conversions here as we need to allow lists
  ## put all conversions inside the Shiny app 
  
  rownames(dataset)<- 1:nrow(dataset)
    gm$distance   <- NULL
    gm$partitions <- NULL
    gm$lensevals  <- NULL #lense vals coloring
    gm$clusters   <- NULL
    gm$nodes      <- NULL
    gm$adjmatrix  <- NULL
    gm$groups     <- list()
    
  return(gm)
}

distance.graphmapper <- function(gm, method="euclidean") {
  if ( gm$normalize_data ) {
    d = scale(gm$d) 
    }
  else {
    d = gm$d
  }
  
  dist(d,method="euclidean", upper=FALSE)
  
}
  
#' partition a graphmapper object dataset by reducing dimensions via lense or filter function
#' lense function must be defined in the graphmapper object, and must return vector with 
#' rownames preserved
#' @param gm graphmapper object with dataset and lense function and lense parameters
#' @return a list of vectors of rownames from the dataset, e.g. subsets or partitions 
#' @export
partition.graphmapper <- function(gm) {
  if (class(gm) != "graphmapper") stop("partition: requires input of class graphmapper class")
  
  # rename parameters in ojbect for readability
  n <- gm$partition_count # num of partitions 
  o <- gm$overlap         # percent overlap

  
  # apply the lense function and add names
  # L is vector of filtered values (1-d)
  # as a by product this may create the distance matrix ?
  # all we usually need is some way to obtain or calculate a distance matrix
  
  L <- gm$lensefun(gm, gm$lenseparam)
  
  # assume L is in same order as data, transfer row names to keep identity
  names(L) <- rownames(gm$d)

  ### setup parameters for partitioning
  total_length = max(L) - min(L)

  # special case if filter only contains one value for all points
  # create one partition to fill with all filter values
  if (total_length < 1e-15) {
    gm$partition_count = 1
    return(list(L))
  } else {
    # pl= partition length
    pl = total_length/(n - ((n-1)*o))
    p0 = min(L)
  
    
    ## get values for partition i; used by vectorized apply
    partition_values = function(i){
      partition_start = p0 + (pl * (i - 1) * (1-o))  # offset== starting value is 1/2 partition size X parttion number
      partition_end   = partition_start + pl
      return(L[L >=  partition_start & L < partition_end ])
    }
    
    ## test function used to remove empty partitions
    non_empty = function(x) { length(x)>0}
    
    # TODO: parallelize with plyr llply?
    partitions = lapply(1:n,partition_values)
  
    # Note : here is a test that all rows have been included in at least one partition
    # if(nrow(gm$d) != length(unique(unlist(partitions)))) stop("partitioning does not include all rows")
    
    # return with all empty partitions returned
    return(partitions[sapply(partitions, non_empty)])

  } 
}

partition2d.graphmapper <- function(gm) {
  if (class(gm) != "graphmapper") stop("partition: requires input of class graphmapper class")
  
  ### for n>1 dimensions,e all params are lists 
  
  ### starting with creating explicit 2d variables
  ## TODO: generalize all of this to use lists of any dimension
  
  # rename variables for readability
  # TODO use unlist to convert to numeric vectors  ; which works on vectors and scalars too!
  # n = unlist(gm$partition_count)
  # o = unlist(gm$overlap)
  
  n_d1 <- gm$partition_count[[1]] # num of partitions 
  n_d2 <- gm$partition_count[[2]] # num of partitions  
  o_d1 <- gm$overlap[[1]]   # percent overlap
  o_d2 <- gm$overlap[[2]]   # percent overlap
  
  # results of running the each of the lenses]
  # note : each lense (dimension) comes back as a numeric vector
  # TODO : use plyr to llply functions in gm listfun to gm$d and 
  L1 = gm$lensefun[[1]](gm$d, gm$lenseparam[[1]])
  L2 = gm$lensefun[[2]](gm$d, gm$lenseparam[[2]])
  
  # assume L is in same order as data, transfer row names to keep identity
  names(L1) <- rownames(gm$d)
  names(L2) <- rownames(gm$d)
  
  # partition length = linear distance
  # TODO: vectorized version is unlist(lapply(L,max)) - unlist(lapply(L,min))

  total_length_d1 = max(L1) - min(L1)
  total_length_d2 = max(L2) - min(L2)
  
  pl_d1 = total_length_d1/(n_d1 - ((n_d1-1)*o_d1))
  pl_d2 = total_length_d2/(n_d2 - ((n_d2-2)*o_d2))
  
  
  # [0:(n-1)]
  # pl 
  p0_d1 = min(L1)
  p0_d2 = min(L2)
  
  partitions = list()
  partition_index = list()
  index = 1
  for (j in 1:n_d1) { 
   
    partition_start_d1 = p0_d1 + (pl_d1 * (j - 1) * (1-o_d1))  # offset== starting value is 1/2 partition size X parttion number
    partition_end_d1   = partition_start_d1 + pl_d1
    for (i in 1:n_d2) {
      print(j)
      partition_start_d2 = p0_d2 + (pl_d2 * (i - 1) * (1-o_d2))  # offset== starting value is 1/2 partition size X parttion number
      partition_end_d2   = partition_start_d2 + pl_d2
      ##L[L >=  partition_start_d1 & L < partition_end_d1,]
      ## each partition is a list, one item for each dimension.   
      ## rowset is intersection of rows that fit in both dimensions
      ## the code below is incorrect
      rowset = list(L1[L1 >=  partition_start_d1 & L1 < partition_end_d1], L2[L2 >=  partition_start_d2 & L2 < partition_end_d2] )
      
      ## TODO only add to partitions if non empty
      partitions[[index]] = rowset
      ## todo : store current (i,j) coordinates of this partition in seperate list
      partition_index[[index]] = c(i,j)
        ## list(L1[L1 >=  partition_start_d1 & L1 < partition_end_d1], L2[L2 >=  partition_start_d2 & L2 < partition_end_d2] )
      index = index + 1
      
    }
  }
  # Note : here is a test that all rows have been included in at least one partition
  # if(nrow(gm$d) != length(unique(unlist(partitions)))) stop("partitioning does not include all rows")
  
  return(partitions)
  
} 


#' Mapper clustering of each partition using histogram method
#' this function creates a list with same length as partitions, 
#' each list item containing vector, with same labels/names as rowids,  of cluster ids from cutree 
#' function
# eg. for partition[[1]];  gmclusts[[1]]  = 
# 114 115 116 117 126 127 133 134 143 145 
# 2   1   2   2   2   2   2   1   2   3
#' @param gm graphmapper object
#' @param distance_method how is the distance matrix computed, default euclidean
#' @param index_method clustering method default single linkage 
#' @param scaling scale the data prior to calculating the distance matrix True/False
#' @return list for each partition of vectors as returned from cutree, cluster groups with names for dataset rows
#' eg. for partition[[1]];  gmclusts[[1]]  = 
#' 114 115 116 117 126 127 133 134 143 145 
#' 2   1   2   2   2   2   2   1   2   3
#' to be used by mapper node creation function
#' @export
clusters.graphmapper<- function(gm, cluster_method = "single", scaling=FALSE, shinyProgressFunction = NULL) {
  
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


clusters2d.graphmapper<- function(gm, cluster_method = "single", scaling=FALSE, shinyProgressFunction = NULL) {
  
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


#' histogram based cut function for single-linkage clusters
#' @export
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

#' create the nodes from clustered partitions in graphmapper object
#' nodes are subsets of IDs (rownames) based on optimal partitions
#' @param gm graphampper object with paritions and clusters
#' @export
nodes.graphmapper <- function(gm){
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
adjacency.graphmapper<- function(gm) {
    # TODO: create an edge list instead and call it 
    # TODO: 
    # gm$edges <- findedges.graphmapper(gm)
  
  # shorten the name
  nodes = gm$nodes
  
  # special case if only one node, adjmat will always be 1x1 with value 0
  # no need to check for overlap
  if (length(nodes) == 1){
    adjmat <- matrix(0)
  } else {
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
  }
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

# assign groups in gm object
#' @export
setgroup.graphmapper <-function(gm,node_ids,group_id = NULL) {
  
  if(!is.graphmapper(gm)) return(NULL)  # raise exception
  
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


has.groups <- function(gm){
  if(is.null(gm$groups)) return(FALSE)
  if(length(gm$groups) < 2) return(FALSE)
  return(TRUE)
}

# return a table of the variance for each variable for each group
#' @export
varTable <- function(gm, group_ids = c(1,2)){
  if(! has.groups(gm)) {return(NULL)}
  varFun <- function(varname){
    d1 = groupdata(gm,group_ids[1],varname)
    d2 = groupdata(gm,group_ids[2],varname)
    return(data.frame("var"=varname, "mean group 1"=mean(d1),  "variance group 1" = var(d1), "mean group 2"=mean(d2), "variance group 2" = var(d2)))
  }
  vtable = ldply(colnames(gm$d), varFun)
}

# returns a table of ks results for each variable in gm$d
#' @export
kstable <- function(gm, group_ids = c(1,2)){
  # requires the 'groups' of the gm object be set ahead of time
  # could have more than 2 , allow to select 2 groups 
  # if(!has.groups(gm)) stop("requires groups to run test") # raise exception need some groups!
  
  # inner function for apply
  ksfun <- function(varname) {
    # TODO this is duplicating the data in memory
    d1 = groupdata(gm,group_ids[1],varname)
    d2 = groupdata(gm,group_ids[2],varname)
    # suppress the ks test warnings because we know there will be ties
    kt = suppressWarnings(
      ks.test(d1,d2, alternatives = "two.sided", exact = FALSE) 
      )
    return(data.frame("var"=varname, "pvalue" = kt$p.value, "kstatistic" = kt$statistic))
  }
  print('making table')
  
  vars = colnames(gm$d)
  ktable = ldply(vars, ksfun)
  
  return(ktable[order(ktable$pvalue),])
}


# returns the data rows for a given group id
groupdata <- function(gm, group_id, varname = NULL){
  # TODO : add error testing
  nodedata(gm, gm$nodes[gm$groups[[group_id]]], varname)
}


####### helpers

is.graphmapper <- function(o){
  return((class(o)=="graphmapper"))
}

is.varname <- function(gm, varname){
  return( Reduce("&", (varname %in% names(gm$d))))
}

# this is dangerous practice
# but returns a variable name in the data set no matter what is sent
guaranteedVarname <- function(gm,  varname=NULL){
  if(!is.graphmapper(gm)) return(NULL)
  
  if (is.null(varname))   return(colnames(gm$d)[1])
  
  if( Reduce("&", (varname %in% colnames(gm$d)))) return(varname)

  if (varname %in% lenseChoices) return(varname)

  return(names(gm$d)[1])
}

# return data rows by variable for given node
# a node is a list of data row IDs from gm$d, note a node ID (node 3)
#' @export
nodedata <- function(gm, nodes, varname=NULL){
  if(!is.graphmapper(gm)) return (NULL)

  # unlisting potentially overlapping nodes, this works on single node, too
  rowids = unique(unlist(nodes))
  
  # if no variable name sent, return all columns
  if(is.null(varname)){
    return(gm$d[rowids,])
  } 
  else {
    # return only column(s) requested in varname
    # use reduce here to combine TRUES if varname is vector of names c("X", "Y")
    if( Reduce("&", (varname %in% colnames(gm$d)))){
      return(gm$d[rowids,varname])
    } else if( (varname %in% lenseChoices)){
      return(gm$lensevals[rowids,])
    }
  }
  return()
}

# returns one or more columns of data as listed in one or more partitions
#' @export
partitiondata <- function(gm, p, varname = NULL){
  if(!is.graphmapper(gm)) return (NULL)
  
  # convert list of partitions into single vector of row ids
  p = unique(unlist(p))
  
  # if no varname param, return all columns
  if(is.null(varname)){
    return(gm$d[gm$partitions[[p]],])
  } 
  else {
    # use reduce() here to allow a vector of column names
    if (Reduce("&", (varname %in% names(gm$d))))
        return(gm$d[gm$partitions[[p]],varname])
  }
  # bad variable name sent - error condition?
  return(NULL)
}


####### LENSES



########## plotting 


plot.graphmapper <- function(gm){
  if(class(gm) != "graphmapper"){ stop("requires graphmapper object")}
  # create an edge list
  adjmatrix = cedar.adj(gm)
  cedar.graph(adjmatrix) 
}

plot_partitions <- function(gm,varx,vary)  {
  partitions = gm$partitions
  par(mfrow=c(2,2)) # set for 2X2 plot
  for(i in 1:length(partitions)){ with(partitions[[i]], plot(gm$d[[varx]],gm$d[[vary]])) }
  par(mfrow=c(1,1))
  
  par(mfrow=c(2,2))
  
  #for(p in partitions){ 
    # remove the ID column,TODO remove hard coded col num
     
  #  print( eclust(gm$d[p,], FUNcluster="hclust", k.max = 5, stand =TRUE, B = 500, hc_metric="euclidean", hc_method="single"))
  # }
}    

# TO DO: refactor to plot arbitrary columns
# plot_cluster = function(gm, x, y, cnumber){
#  cldata = cbind(rowid = gm$partitions[[cnumber]],clusterid = gm$clusters[[cnumber]])
# SET X AND Y variables  FROM DATA
#  plot(cldata$X, cldata$Y, col = c("red", "green", "blue")[rowid])
  # d[d$ID %in% names(cl[[1]]),]
# }

