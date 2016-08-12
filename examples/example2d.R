
simple_lense = function(d,varname=NULL ){
  # single column lense
  
  # simple lense function that returns a single variable
  # d = data as a data frame, varname  = name of variable as a string
  
  # TO DO: always use the first column and let the caller send the column of interest
  # if no variable name passed, use the first name
  # requires data d to have named columns
  if (is.null(varname)) { varname = names(d)[1] }
  
  # prep data frame for partitioning function with L column 
  lense_df = data.frame("V1" = d[,1],"V2" = d[,2])
  return(lense_df)
  
}



# example scripts of creating and using a Mapper object
#library(cedar)
library(plyr)

strt<-Sys.time()

# example scripts of creating and using a Mapper object

data(chemdiab)
chemdiab  <- subset(chemdiab, select = -c(cc))


gm <- graphmapper(dataset=chemdiab, 
                  lensefun=list(lense.projection,lense.projection), 
                  partition_count=list(6,6) 
                  overlap = list(0.5,0.5), 
                  cluster_method='single', 
                  bin_count=list(15,15), 
                  lenseparam=list("rw","fpg"),
                  normalize_data=TRUE,
                  dimensions = 2)

#gm  = graphmapper(dataset = chemdiab, 
#                    simple_lense, 
#                    partition_count = 5,
#                    partition_d2 = 5,
#                    overlap_d1 = 0.5, 
#                    overlap_d2 = 0.5, 
#                    cluster_method="single", 
#                    lenseparam = NULL,
#                    bin_count=5 
# )

gm$partitions = partition2d.graphmapper(gm)
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


