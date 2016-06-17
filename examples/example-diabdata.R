# example scripts of creating and using a Mapper object
library(cedar)
library(plyr)

data(chemdiab)
chemdiab  <- subset(chemdiab, select = -c(cc))


gm  = graphmapper(dataset = chemdiab, 
                    simple_lense, 
                    partition_count=4, 
                    overlap = 0.5, 
                    partition_method="single", 
                    index_method="gap",  
                    lenseparam = names(chemdiab)[1]
                  )

gm$partitions = partition.graphmapper(gm)
gm$clusters   = clusters.graphmapper(gm, 100 ) 
gm$nodes      = nodes.graphmapper(gm)
gm$adjmatrix  = adjacency.graphmapper(gm) 


# use gui or other method for selecting groups


gm[["groups"]] <- setgroup.graphmapper(gm, c(1,2,3),group_id = "1")
gm[["groups"]] <- setgroup.graphmapper(gm,c(5,6,7,8,9), group_id = "2")

# demo of adding to a group.  setgroup appends to existing group
gm[["groups"]] <- setgroup.graphmapper(gm, c(4), "1")

# TURN OFF WARNINGS
kt = kstable(gm)
print(kt)
print(varTable(gm))

