# helper_mapper.R
# helper functions for CEDAR testing framework

example_lense  <- function(){
  lense(lense.constant, lenseparam=NULL, partition_count=4, overlap = 0.5)
}

projlense <- function(varname,p=4,o=0.5){
  lense(lense.projection, varname, partition_count=p, overlap = o)
}

circle_mapper <- function(npoints=60,partition_count=4, overlap = 0.5){
  d = circle_data(r=1,n=npoints)
  lenseparam = "X"
  l = lense(lense.projection, lenseparam, partition_count, overlap) 
  mapper(dataset = d, 
         lenses=list(l), 
         cluster_method="single", bin_count=10, normalize_data=FALSE)
}


example_mapper <- function(){
  d <- circle_data()
  lenseparam = "X"
  l = lense(lense.projection, lenseparam, partition_count=4, overlap = 0.5) 
  mapper(dataset = d, 
         lenses=list(l), 
         cluster_method="single", bin_count=10, normalize_data=FALSE)
}

example_mapper_2d <- function(){
  d <- circle_data()
  l1 <- lense(lense.projection, lenseparam="X", partition_count=4, overlap = 0.5) 
  l2 <- lense(lense.projection, lenseparam="Y", partition_count=4, overlap = 0.5) 
  
  mapper(dataset = d, 
         lenses=list(l1,l2), 
         cluster_method="single", bin_count=10, normalize_data=FALSE)
}

grid_1d_mapper <- function(n=10, partition_count=3,var="X"){
  mapper(dataset = grid_data(n), 
         lenses = list(projlense(var,partition_count)), 
         cluster_method="single", 
         bin_count=10, normalize_data=FALSE)
}

grid_2d_mapper <- function(n=10, partition_count=3){
  mapper(dataset = grid_data(n), 
         lenses = list(projlense('X',partition_count),projlense('Y',partition_count)), 
         cluster_method="single", 
         bin_count=10, normalize_data=FALSE)
}


integer_data <- function(n,start=1){
  d<- data.frame("X" = 1:n)
  rownames(d) = 1:nrow(d)
  return(d)
}

integer_mapper <- function(n=10, partition_count=3,start=1){
  mapper(dataset = integer_data(n), 
         lenses = list(projlense('X',partition_count)), 
         cluster_method="single", 
         bin_count=10, normalize_data=FALSE)
}

decimal_data <- function(n){
  data.frame("X" = sapply(c(0,1:n),FUN=function(i){i/n}))
}


