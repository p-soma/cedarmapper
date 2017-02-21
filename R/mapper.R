# mapper.R
# This is the main object defs and  pipeline for n-lense mapper pipeline

#' @import cluster
library(cluster)

# SETUP GLOBAL VAR for debugging when necessary
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
mapper <- function(dataset, lenses, lensevals=NULL, cluster_method="single", bin_count=10, normalize_data=TRUE, selected_cols=names(dataset)){
  # lenses have previous parameters used: lensefun, partition_count=4, overlap = 0.5,lenseparam = NULL
  
  # note: dimensions variable 
  # note: using as.numeric to convert arguments becuase Shiny inputs return strings
    
  #  "partition_count"=as.numeric(partition_count), 
  #  "overlap" = as.numeric(overlap),   # percent, o <= 1
   gm = structure(  list(d = dataset, 
                      "lenses"=lenses, 
                      "cluster_method"=cluster_method, 
                      "bin_count" = as.numeric(bin_count),
                      "normalize_data" = normalize_data,
                      "selected_cols" = names(d),
                      "lensevals"=lensevals
                      ),
                 class="mapper")
  
  ### TODO : ensure all as.numeric conversions of parameters inside the Shiny app 
  
  rownames(dataset)<- 1:nrow(dataset)
    gm$distance   <- NULL
    gm$partitions <- NULL
    gm$clusters   <- NULL
    gm$nodes      <- NULL
    gm$adjmatrix  <- NULL
    gm$groups     <- list()
    gm$lensevals  <- NULL
    
  return(gm)
}

#' return the dimension of a graph mapper object by counting lenses
#' @param gm graphmapper object
#' @return integer >1 or NA
#' @export
mapper.dimensions <- function(gm){
  length(gm$lenses)
}

# single method to run all steps for mapper pipeline given a mapper object
#' @export
mapper.run <- function(m, progressUpdater = NULL){
  # TODO : means to determine if data has changed, and if not, don't recalculate distance matrix
  m$distance   <- distance.mapper(m,method="euclidean") # dist(scale(gm$d),method="euclidean", upper=FALSE)
  m$partitions <- partition.mapper(m)
  m$clusters   <- clusters.mapper(m, shinyProgressFunction=progressUpdater ) 
  m$nodes      <- nodes.mapper(m)
  m$adjmatrix  <- adjacency.mapper(m) 
  m$lensevals  <- data.frame(mapper.lense.calculate(m)$values)
  return(m)
  
}

#  single method to collect parameters and then run all steps for 1D mapper object
#  
#' @export
makemapper <- function(dataset, lensefun, lensevals=NULL, partition_count=4, overlap = 0.5,  
                       bin_count=10, cluster_method= 'single', lenseparam = NULL, 
                       normalize_data=TRUE, progressUpdater=NULL, selected_cols=names(dataset)){
  # create objects with the above params
  one_lense <- lense(lensefun, lenseparam, partition_count, overlap)
  gm <- mapper(dataset=dataset, 
               lenses=list(one_lense),
               cluster_method=cluster_method, 
               bin_count=bin_count, 
               normalize_data=normalize_data,
               lensevals=lensevals,
               selected_cols=selected_cols
               )
  gm <- mapper.run(gm)
#  gm$lensevals
  return(gm)
}

#' calculate distance matrix of the existing data, scale if normalize data is checked
#' @param gm A mapper object with d data member
#' @param method A string method name used by the dist() function
#' @return distance matrix of data element of mapper object gm$d
distance.mapper <- function(m, method="euclidean") {
  # same method, just using scaled data or not
  if ( m$normalize_data ) 
    { dist(scale(m$d[, m$selected_cols]),method, upper=FALSE)  }
  else 
    { dist(m$d[, m$selected_cols],method, upper=FALSE) }
}
  

#"lensefun"  = lensefun, 
#"lenseparam"= lenseparam,  # don't use as.numeric here, sometimes is variable name
#"n"         = as.numeric(partition_count),  # or should be n
#"o"         = as.numeric(overlap)),
#"p0"        = NULL, # lower bound of lense values, filled in when L values calculated
#"pl"  = NULL,  # partition length

#' calculate the lense function values and parameters needed for partitioning
#' mapper object is needed for data and distance matrix only
#' @export
#' @param gm
#' @return Lense object with values and stats
mapper.lense.calculate <- function(m,dimension=1){
  L <- m$lenses[[dimension]]
  if (class(L) != "lense") {stop ("partition function requires a lense object")}
  
  # fill up L member variables and return it
  # L$values is 1D vector of values from the filter/lense function with same length as mapper data
  L$values <- L$lensefun(m$d[, m$selected_cols], L$lenseparam, m$distance)
  names(L$values) <- rownames(m$d)
  
  # calc and store aspects of resulting vector; could be expensive
  L$p0 <- min(L$values)  
  L$pmax <-max(L$values)
  # partition length (e.g width in terms of L values)
  L$pl <- (L$pmax - L$p0) / (L$n - ((L$n-1)*L$o))
  return (L)
  
}

# 'given a partition index, what is lower bound of that partition
partition_start <- function(partition_index,L){
  if (class(L) != "lense") { stop ("partition function requires a lense object")}
  if (is.null(L$p0)) { stop("error, did not calculate lense") }
  
  if (partition_index > L$n || partition_index < 1 ) { return(NA)}
  return( L$p0 + (L$pl * (partition_index - 1) * (1-L$o)) )
  # offset== starting value is 1/2 partition size X parttion number
}

# partition_starts <- Vectorize( partition_start, c("partition_index"))

# 'given a partition index, what is upper bound of that partition
partition_end <- function(partition_index,L) {
  return( partition_start(partition_index,L) + L$pl )
}


# 'given start value a partition, return the index of that partition
get_partition_index <- function(partition_start_value,L){
  
  if (class(L) != "lense")          { stop("partition function requires a lense object")}
  if (is.null(L$p0) || is.na(L$p0)) { stop("error, did not calculate lense") }
  # L must have been 'calculated' to load these values
  
  # boundary conditions
  if( partition_start_value < L$p0 && partition_start_value > L$pmax ) { return(NA)}
  # if( partition_start_value > (L$p0 + (L$pl * L$n))) {return(NA)}
  
  i = (  (( partition_start_value - L$p0 ) / L$pl ) + (1-L$o)) / (1-L$o) 
  # if(i > L$n + 1 ) { i = NA }
  return(i)
}

get_partition_indices <- Vectorize(get_partition_index)

# given a value from a Lense, which partitions is it in?
partition_index_for_l_value <- function(l_value,L){
  
  if (class(L) != "lense") {stop ("partition function requires a lense object")}
  if (is.null(L$p0) || is.na(L$p0)) { stop("error, did not calculate lense") }
  
  # first use the index calculator on this l_value that's greater than the Partition start, so will have fractional part
  index_plus_something  = get_partition_index(l_value,L)
  partindex = floor(index_plus_something)
  
  # the index is the nearest integer to this calculation
  if( partindex > L$n ) { 
    partindex  <- L$n 
    names(partindex)= names(l_value)
  
  } else {
   
    names(partindex)= names(l_value)
     # when partitions overlap, does this Lense value fit in the lower partition?
     # if it's more than the overlap away from parition value, 
     # it's past end of previous partition; assign second value
  
    distance_from_partition_start = index_plus_something - partindex
    if ( ( partindex > 1) && ( distance_from_partition_start <= L$o ) ) { 
      partindex =   c(partindex, partindex - 1 )
    }   
  }
  
  # TODO add 3rd index when  o > .5 
  
  return( partindex )
  
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
partition.mapper <- function(m) {
  
  if (class(m) != "mapper") stop("partition: requires input of class mapper class")
  if (class(m$lense[[1]]) != "lense") stop ("partition function requires a lense object")
  if (is.null(m$distance)) { m$distance = distance.mapper }  ## TODO this is not saved as pass by value which is inefficient
  
  # calculate lense values and store back in lense objects
  # does NOT save lense calc values into the mapper object
  for(i in 1:length(m$lenses)){
    m$lenses[[i]]= mapper.lense.calculate(m,i)
  }
  
  ## functions used by apply statements to generate lists
  ## use local m mapper object
  # get partition membership for one lense, one row
  getpartlists <- function(d,i){
      partition_index_for_l_value(m$lenses[[d]]$values[i],m$lenses[[d]])
  }

  # determine paritition membership, for each lense/dimension
  accumulate_partlists <- function(i){
      lapply(1:mapper.dimensions(m),getpartlists,i)
  }

  # for each row, take cross product of all dimension partitions  
  partitioncrossproduct <- function(pl){
    cp_df = expand.grid(pl)
    cp_df$id<- names(pl[[1]])  #TODO assigns the row ID ; use names here instead
    return(cp_df)
  }
  
  # list of all partitions, per dimension
  partlists <- lapply(1:nrow(m$d),accumulate_partlists  )
  # list of dataframes of cross products for each row, same size as data
  cplists   <- lapply(partlists,partitioncrossproduct)
  dfparts   <- plyr::rbind.fill(cplists)
  partitions <- split(dfparts$id,dfparts[,names(dfparts)!="id"])
  
  ## TODO either modify the structure above or next step in pipeline
  # this is a list of vectors, each vector a set of rowids. 
  # however it's not compatible with next clustering step. 

  # remove empty partitions
  for(p in names(partitions)){ 
    if(length(partitions[[p]]) == 0 ) { partitions[[p]] <- NULL}
  }
  
  return(partitions)

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
clusters.mapper<- function(m, shinyProgressFunction = NULL) {
  
  # TODO: remove cluster_method param and use gm object variable; ensure gm object has this set...

  gmClusts = list() 
  npartition = length(m$partitions)
  # loop through each partition
  for ( i in 1:npartition) {
    print(paste0("analyzing partition ", i))
    
    # check for special case of only one datapoint, so no clustering necessary, break out of loop
    if(length(m$partitions[[i]]) < 2 ){
      gmClusts[[i]] = c(1)
      names(gmClusts[[i]]) = rownames(m$partitions[[i]], gm$selected_cols)
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
    
    rowset = m$d[m$partitions[[i]],]  # partition is a set of rownames
    # calculate distance matrix for this partition
    
    # TODO: subset the full distance matrix instead of re-calculating here each time
    method='euclidean'
    if ( m$normalize_data ) 
      { partition_dist <- dist(scale(rowset),method)  }
    else 
      { partition_dist <- dist(rowset, method) }
    
    # if(DEBUG) {print(max(partition_dist))}
    # do standard clustering and cut 
    partition_cluster <- hclust(partition_dist, method=m$cluster_method) 
    cluster_cutheight <- cut_function(partition_cluster$height,  max( partition_dist), m$bin_count)
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

