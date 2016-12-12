# mapper.plot.R
# plotting functions for mapper objects

plot.mapper <- function(gm){
  if(class(gm) != "mapper"){ stop("requires mapper object")}
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
