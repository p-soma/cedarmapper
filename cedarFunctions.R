# cedarFunctions.R
# cedar project development functions

library(locfit)
data("chemdiab")

discoverLinks <- function(x){
  # given x = list of subsets of a data source
  #  [ 1 = (3,4,5),2 =  (4,5,6,7), 3= (1,2,3), 4=(5,4,2)]
  # discover which subsets share values 
  # and output list of links
  # 1, 2
  # 1, 3
  # 1, 4
  # 2, 4
  # 3, 4

  }

randomLense <- function(x,groupSize=4,nGroup = 10){
  # given x data.frame
  # create collection of random subsets
  
  
  randomsets= []
  for (i in seq(1,nGroup)) {
  
    randomsets[i] = x[sample(nrow(x), groupSize), ]
  }
  # make this a data.frame  or list of vectors
  return(randomsets)
  
  # future: need to use apply() functions to apply multiple times
  # number of times should be based on nrow(x) and groupSize to guarantee some overlap
  #   and use of most of the data
  
}

