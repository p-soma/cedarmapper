# example scripts of creating and using a Mapper object
library(cedar)
library(plyr)

data(chemdiab)
chemdiab  <- subset(chemdiab, select = -c(cc))


gm  = graphmapper(x = chemdiab, 
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

gm$groups = list(group1 = c(1,2,3,4), group2=c(5,6,7,8,9))

# TURN OFF WARNINGS
kt = kstable(gm)
print(kt)
