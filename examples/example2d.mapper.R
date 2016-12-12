

# example scripts of creating and using a Mapper object
library(cedar)
library(plyr)

strt<-Sys.time()

# example scripts of creating and using a Mapper object

data(chemdiab)
chemdiab  <- subset(chemdiab, select = -c(cc))

lense2 <- lense(lense.projection, lenseparam="rw", partition_count=4, overlap = 0.5) 
lense1 <- lense(lense.density, lenseparam=1, partition_count=10, overlap = 0.5)

gm <- mapper(dataset=chemdiab, lenses=list(lense1,lense2), cluster_method="single", bin_count=10, normalize_data=TRUE)
gm$distance   <- distance.mapper(gm)
gm$partitions <- partition.mapper(gm)

# gm$clusters   = clusters2d.graphmapper(gm) 
# gm$nodes      = nodes2d.graphmapper(gm)[[1]]
# gm$nodehash = nodes2d.graphmapper(gm)[[2]]
# gm$adjmatrix  = adjacency2d.graphmapper(gm) 

# print(gm$nodes)

# use gui or other method for selecting groups

# gm[["groups"]] <- setgroup.graphmapper(gm, c(1,2,3),group_id = "1")
# gm[["groups"]] <- setgroup.graphmapper(gm,c(5,6,7,8,9), group_id = "2")
# 
# # demo of adding to a group.  setgroup appends to existing group
# gm[["groups"]] <- setgroup.graphmapper(gm, c(4), "1")

# TURN OFF WARNINGS
# kt = kstable(gm)
# print(kt)
# print(varTable(gm))
print(Sys.time()-strt)


