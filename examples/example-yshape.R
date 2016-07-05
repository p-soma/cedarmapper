#' Y-shaped data example
#' Synthetic x,y data in shape of Y when plotted for testing Mapper output
#' 

library(cedar)

testdata = y_data(100)
plot(testdata, main="Our example Y-shaped, x-y data")

cat ("Press [enter] to run Mapper and plot")
line <- readline()

# create graphmapper object, set lense to be the density function
gm = graphmapper(dataset = testdata, lensefun=lense.density,  lenseparam = 0.5, 
                 partition_count=10, overlap = 0.5, bin_count=15)

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

# set groups by dividing nodes in half (arbitrarily)
midnode = floor(length(gm$nodes)/2)
gm[["groups"]] <- setgroup.graphmapper(gm, 1:midnode,group_id = "1")
gm[["groups"]] <- setgroup.graphmapper(gm, midnode+1:length(gm$nodes),group_id = "2")
print(kstable(gm))

# example 2, using the 'maker' function to create mapper object in one step

# gm2  = makegraphmapper(dataset = testdata, lensefun=lense.projection,  lenseparam = 'y',
#                partition_count=4, overlap = 0.5, bin_count=10)
# plot(graph.graphmapper(gm2),main="Y-data mapper graph")
