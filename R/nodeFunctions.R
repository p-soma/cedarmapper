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

#  single method to run all steps for graphmapper object
#' @export
makegraphmapper <- function(x, lensefun, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap", lenseparam = NULL, progressUpdater=NULL){
  # create object with params 
  gm = graphmapper(x, lensefun, partition_count, overlap, partition_method, index_method, lenseparam)
  
  # create partitions 
  gm$partitions = partition.graphmapper(gm)

  # create clusters in each partition; this takes a while depending on number of iterations
  # TODO parameterize iterations (currently hard codes)
  # note, the progressUpdater construct is for ShinyApps and optional
  gm[["clusters"]]   = clusters.graphmapper(gm, 100,progressUpdater ) 
  
  # create nodes from clusters 
  gm[["nodes"]]     = nodes.graphmapper(gm)

  # build links of overlapping nodes as an adjancy matrix
  gm[["adjmatrix"]] = adjacency.graphmapper(gm) 

  return(gm)
  
}


# graphmapper class factory
#' @export
graphmapper <- function(x, lensefun, partition_count=4, overlap = 0.5, partition_method="single", index_method="gap", lenseparam = NULL){
  # note: using as.numeric to convert arguments becuase Shiny inputs return strings
  gm = structure(list(d = x, 
                      "partition_count"=as.numeric(partition_count), 
                      "overlap" = as.numeric(overlap),   # percent, o <= 1
                      "lensefun"=lensefun, 
                      "partition_method"=partition_method, 
                      "index_method"=index_method, 
                      "lenseparam" = lenseparam),
                 class="graphmapper")
    gm$partitions = NULL
    gm$clusters   = NULL
    gm$nodes      = NULL
    gm$adjmatrix  = NULL
  return(gm)
}


#' @export
partition <- function(d, lensefun, n=4, o=0.5, lenseparam=NULL){
  # calculate lense values for all rows in d, use optional parameter
  # lensefun must return data.frame with columns L(values) and ID (rownames)
  
  lense.df = lensefun(d, guaranteedVarname(lenseparam))
  # debug
  # print(summary(lense.df))
  
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
  print(paste("gm values=", gm$partition_count,  gm$overlap, gm[["lenseparam"]]))
  
  if (class(gm) != "graphmapper") stop("partition: requires input of class graphmapper class")
  return(partition(gm$d, gm$lensefun, gm$partition_count,  gm$overlap, gm[["lenseparam"]]))
    
  # debug
  # for(i in length(gm["partitions"])) {print(paste0("parition ", i, " sized ", length(gm["partitions"][[i]])))}
} 


# cluster detection using NbClust; 
# the clustering built in so works on partitions
#' @export
clusters.graphmapper<- function(gm, iterations=250, shinyProgressFunction = NULL) {
  
  # TODO : use gm object values as parameters for these 
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
  
  npart = length(gm$partitions)
  for ( i in 1:npart) {
    # if (debug) print(i)
    # subset of data for this partition, which contains row names...
    # this is not necessary here, and adds to R memory burden for large partitions
    # but adds to code readability
    # gf = gapFunction(p)
    print(paste0("analyzing partition ", i))
    
    # If we were passed a shiny progress update function, call it for each loop
    if (is.function(shinyProgressFunction)) {
      text <- paste0("running ", iterations, " cluster iterations for partition ", i)
      shinyProgressFunction(value = (i/npart), detail = text)
     }
    
    x = as.matrix(gm$d[gm$partitions[[i]],])
    print(nrow(x))
    # alternative method to using clusgap above
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
  
  if (is.null(varname))   return(names(gm$d)[1])
  
  if( Reduce("&", (varname %in% names(gm$d)))) return(varname)

  return(names(gm$d)[1])
}

# return data rows by variable for given node
# a node is a list of data row IDs from gm$d, note a node ID (node 3)
#' @export
nodedata <- function(gm, nodes, varname=NULL){
  if(!is.graphmapper(gm)) return (NULL)

  # unlisting potentially overlapping nodes, this works on single node, too
  nodes = unique(unlist(nodes))

  # if no variable name sent, return all columns
  if(is.null(varname)){
    return(gm$d[nodes,])
  } 
  else {
    # return only column(s) requested in varname
    # use reduce here to combine TRUES if varname is vector of names c("X", "Y")
    if( Reduce("&", (varname %in% names(gm$d))))
      return(gm$d[nodes,varname])
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
#' @export
simple_lense = function(d,varname=NULL ){
  # single column lense
  
  # simple lense function that returns a single variable
  # d = data as a data frame, varname  = name of variable as a string
  
  # TO DO: always use the first column and let the caller send the column of interest
  # if no variable name passed, use the first name
  # requires data d to have named columns
  if (is.null(varname)) { varname = names(d)[1] }
  
  # prep data frame for partitioning function with L column 
  lense_df = data.frame(L=get(varname, d), ID=rownames(d), stringsAsFactors = FALSE)
  return(lense_df)
  
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

#' @export
lense.density <- function(d,varname=NULL){
  # single column lense; only first column is used
  if (is.null(varname)) { varname = names(d)[1] }
  
  # kernel density using params from params object
  # params object ignored for now using standard values
  #if(is.null(params$bw)) params$bw= "nrd"
  #if(is.null(params$kernal))params$kernel = "gaussian"
  bw = "SJ"
  kernel = "gaussian"
  dens_fun = approxfun(density(d[[varname]], bw="SJ",kernel="gaussian"))
  lense_df = data.frame(L=dens_fun(d[[varname]]), ID=rownames(d), stringsAsFactors = FALSE)
  return(lense_df) 
}


#' @export
lense.pca <- function(d,varname=NULL) {
  # multiple column lense, ignore var parameter
  d.pca = prcomp(d, retx=TRUE, center=TRUE, scale. = TRUE)
  data.frame(L=d.pca$x[,"PC1"], ID=rownames(d), stringsAsFactors = FALSE) 
}

#' @export
lense.distance <- function(d,varname=NULL) {
  # multi-column lense, ignore var parameter
  data.frame(L=mahalanobis(scale(d, center=TRUE,scale=TRUE ), center=colMeans(d), cov=cov(d)), stringsAsFactors = FALSE)
}

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

