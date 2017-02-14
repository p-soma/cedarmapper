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

