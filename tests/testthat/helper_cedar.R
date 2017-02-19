# helper_mapper.R
# helper functions for CEDAR testing framework

example_lense  <- function(){
  lense(lense.constant, lenseparam=NULL, partition_count=4, overlap = 0.5)
}



example_mapper <- function(){
  d = circle_data()
  lenseparam = "X"
  l = lense(lense.projection, lenseparam, partition_count=4, overlap = 0.5) 
  mapper(dataset = d, 
         lenses=list(l), 
         cluster_method="single", bin_count=10, normalize_data=TRUE)
}

example_mapper_2d <- function(){
  d = circle_data()
  l1 = lense(lense.projection, lenseparam="X", partition_count=4, overlap = 0.5) 
  l2 = lense(lense.projection, lenseparam="Y", partition_count=4, overlap = 0.5) 
  
  mapper(dataset = d, 
         lenses=list(l1,l2), 
         cluster_method="single", bin_count=10, normalize_data=TRUE)
}


projlense <- function(varname){
  lense(lense.projection, varname, partition_count=4, overlap = 0.5)
}

integer_data <- function(n){
  data.frame("X" = 1:n)
}

decimal_data <- function(n){
  data.frame("X" = sapply(c(0,1:n),FUN=function(i){i/n}))
}
