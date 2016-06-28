# example scripts of creating and using a Mapper object
library(cedar)
library(plyr)

data(chemdiab)
chemdiab  <- scale(subset(chemdiab, select = -c(cc)))

testlensefun = lense.density
lenseparam = 1.0

# alternative, single var projection
# testlensefun = simple_lense
# lenseparam = names(chemdiab)[1]


gm  = graphmapper(dataset = chemdiab, 
                    lensefun = testlensefun, 
                    partition_count=6, 
                    overlap = 0.5, 
                    partition_method="single", 
                    index_method="gap",  
                    lenseparam = lenseparam,
                    bin_count=5 
                  )

gm$distance = dist(gm$d,method="euclidean", upper=FALSE)
gm$partitions = partition.graphmapper(gm)
gm$clusters   = clusters.graphmapper(gm) 
gm$nodes      = nodes.graphmapper(gm)
gm$adjmatrix  = adjacency.graphmapper(gm) 

print(gm$nodes)

# use gui or other method for selecting groups

gm[["groups"]] <- setgroup.graphmapper(gm, c(1,2,3),group_id = "1")
gm[["groups"]] <- setgroup.graphmapper(gm,c(5,6,7,8,9), group_id = "2")

# demo of adding to a group.  setgroup appends to existing group
gm[["groups"]] <- setgroup.graphmapper(gm, c(4), "1")

# TURN OFF WARNINGS
kt = kstable(gm)
print(kt)
print(varTable(gm))

