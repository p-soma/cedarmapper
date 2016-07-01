#' Y-shaped data example
#' Synthetic x,y data in shape of Y when plotted for testing Mapper output
#' 

assign("DEBUG",TRUE,.GlobalEnv)

#' create x,y data in shape of reclining Y
#' @export
yshape <- function(n=100){
  r1 = runif(n,-1,0)
  r2 = runif(n,-1,0)
  r3 = runif(n,0,1)
  
  d1 = cbind(x=r1, y=-r1)
  d2 = cbind(x=r2,y=r2)
  d3 = cbind(x=r3,y=0)
  return(data.frame(rbind(d1,d2,d3)))
}


testdata = yshape(100)
plot(testdata, main="Our example Y-shaped, x-y data")

cat ("Press [enter] to run Mapper and plot")
line <- readline()

# create object
gm = graphmapper(dataset = testdata, lensefun=lense.projection,  lenseparam = 'x',
                   partition_count=5, overlap = 0.5, bin_count=10)

# gm pipeline, run manually
gm$distance = dist(gm$d,method="euclidean", upper=FALSE)
gm$partitions = partition.graphmapper(gm)

# print information about the the partitions
for(i in 1:length(gm$partitions)) {print(paste0("parition ", i, " sized ", length(gm$partitions[[i]])))}

gm[["clusters"]]   = clusters.graphmapper(gm) 
gm[["nodes"]]     = nodes.graphmapper(gm)

# print information about the nodes
print(paste0("created ", length(gm$nodes), " nodes"))

# build links of overlapping nodes as an adjancy matrix
gm[["adjmatrix"]] = adjacency.graphmapper(gm) 

# create iGraph and plot
plot(graph.graphmapper(gm))

# example 2, use single function to create mapper object and plot

cat ("Press [enter] to run Mapper")
line <- readline()
gm2  = makegraphmapper(dataset = data.frame(yshape(100)), lensefun=lense.projection,  lenseparam = 'x',
                partition_count=6, overlap = 0.5, bin_count=15)
plot(graph.graphmapper(gm2),main="Y-data mapper graph")
