# example scripts of creating and using a Mapper object


data(chemdiab)
chemdiab  <- subset(chemdiab, select = -c(cc))


gm  = graphmapper(x = datasets[["Diabetes"]], 
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



