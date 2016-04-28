# graphmapper on chemdiab data
library(locfit)

gmdiab<- function(diabvar="rw") {
  data("chemdiab")
  chemdiab.ccremoved = subset(chemdiab, select = -c(cc)) # need to remove the result, leave only numeric columns
  partition_method="single"
  index_method="gap"
  
  gm = structure(list(d =chemdiab.ccremoved, 
                      "partition_count"=4, 
                      "overlap" = 0.5,   # percent, o <= 1
                      "lensefun"=simple_lense, 
                      "partition_method"=partition_method, 
                      "index_method"=index_method), 
                 class="graphmapper")
  
  gm[["lenseparam"]] = diabvar
  
  gm[["partitions"]] = partition.graphmapper(gm)
  gm[["clusters"]]   = clusters.graphmapper(gm,200)
  gm[["nodes"]]     = nodes.graphmapper(gm)
  gm[["adjmatrix"]] = adjacency.graphmapper(gm) 
  
  return(gm)
  
}