# example scripts of creating and using a Mapper object
library(cedar)

data(chemdiab)
# current must remove the classification variable
chemdiab  <- scale(subset(chemdiab, select = -c(cc)))

testlensefun = lense.density
lenseparam = 1.0

# alternative, single var projection
# testlensefun = simple_lense
# lenseparam = names(chemdiab)[1]


gm  = graphmapper(dataset = chemdiab, 
                    lensefun = testlensefun, 
                    partition_count=8, 
                    overlap = 0.5, 
                    lenseparam = lenseparam,
                    bin_count=20 
                  )

# create distance matrix for all data manually
gm$distance = dist(gm$d,method="euclidean", upper=FALSE)
gm$partitions = partition.graphmapper(gm)
gm$clusters   = clusters.graphmapper(gm)
gm$nodes      = nodes.graphmapper(gm)
gm$adjmatrix  = adjacency.graphmapper(gm) 

plot(graph.graphmapper(gm),main=paste0("Diabetes Data, Density function"),sub=paste0("paritions: ", gm$partition_count, ", overlap: ", gm$overlap))

# select 2 disjoint  groups at random
nodecount = length(gm$nodes)
g1 = sample(1:nodecount,nodecount/2)
g2 = setdiff(1:nodecount,g1)

gm[["groups"]] <- setgroup.graphmapper(gm, g1,group_id = "1")
gm[["groups"]] <- setgroup.graphmapper(gm, g2, group_id = "2")

# example of adding to a group.  setgroup appends to existing group
# gm[["groups"]] <- setgroup.graphmapper(gm, c(4), "1")

# TURN OFF WARNINGS
kt = kstable(gm)
print(kt)
print(varTable(gm))

