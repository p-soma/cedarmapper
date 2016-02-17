# cedarFunctions.R
# cedar project development functions

# generate a set of points on circle
# example c = randCircle(); plot(c)

randCircle <- function(r=1, n=60) {
  angles = sample(359,n)
  c = data.frame(X=cos(angles)*r, Y=sin(angles)*r)
  return(c)
}

ylense <- function(rowdata) {
  # 4 groups and 50% overlap
  
  ngroups=4
  o=0.5
  # this function needs to be made generic 
  partition_size = 0.4
  lense.function = function(x) {
    return(data.frame(L=x$Y, ID=x$ID))
  }
  

  # partition_size = round(((nrow(rowdata)/ngroups ) * (1 + o)) + 0.5)
  
  partition_size = round ( ( nrows(rowdata) * 0.4 ) + 0.5 ) 

  lense.partition = function(s) {
    return(lense.d[s:(s+group_size),])
  
  }
  
  # make sure it's a data frame, and add an ID number
  c.df = data.frame(c,ID=seq.int(n))
 
  lense.d = lense.function(c)
  
  
  
  # sort result from lense function
  lense.d = lense.d[with(lense.d, order(L)), ]
  
  # calculate size of partitions
  interval =  group_size * (1-o)

   # to do
  # this doesn't cover all 60 items
  # this doesn't accumulate the groups

  lense.lists = lapply((1:ngroups),lense.partition)
  
}


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


