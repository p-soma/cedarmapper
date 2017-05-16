context("Parititioning")

# see helpder_cedar.R for object creations functions

projectionlense <- function(varname){
  lense(lense.projection, varname, partition_count=5, overlap = 0.5)
}

integer_data <- function(n){
  data.frame("X" = 1:n)
}

decimal_data <- function(n){
  data.frame("X" = sapply(c(0,1:n),FUN=function(i){i/n}))
}

test_that("simple 12 value lense can be partitioned", {
  # this is natural number data set, just to ensure the partition works
  # 5 partitions, 50% overlap, evenly covers points 1-13 
  m <- mapper(d=data.frame("X"=c(1:13)),
              lenses = list(lense(lense.projection, "X", partition_count=5, overlap = 0.5)),
              cluster_method="single", bin_count=10, normalize_data=FALSE)
  m$distance <- distance.mapper(m)
  m$lenses[[1]] <- mapper.lense.calculate(m,1)
  L <- m$lenses[[1]]
  plist <- lapply(L$values, partition_index_for_l_value, L)
  
  known_partition_list = list('1'=1,
                              '2'=1,
                              '3'=c(2, 1),
                              '4'=c(2, 1),
                              '5'=c(3, 2),
                              '6'=c(3, 2),
                              '7'=c(4, 3),
                              '8'=c(4, 3),
                              '9'=c(5, 4),
                             '10'=c(5, 4),
                             '11'=5,
                             '12'=5,
                             '13'=5)
  
  expect_true(all.equal(plist,known_partition_list))

  })


test_that("partition calculations", {
  m <- mapper(d=data.frame("X"=c(1:13)),
                lenses = list(projectionlense("X")),
                cluster_method="single", bin_count=10, normalize_data=FALSE)
  m$distance <- distance.mapper(m)
  m$lenses[[1]] <- mapper.lense.calculate(m,1)
  L <- m$lenses[[1]]
  expect_equal(partition_start(1,L),1)
  expect_equal(partition_start(2,L),3)
  
  partition_indexes = 1:L$n
  # this gives the partition starting vlues
  all_partition_starting_values <- sapply(1:L$n, partition_start, L)
  expect_equal ( sapply( all_partition_starting_values ,get_partition_index,L), partition_indexes)
  
})

test_that("mapper partition calc works",{
  m <- circle_mapper()
  m$distance    <- distance.mapper(m)
  m$partitions  <- partition.mapper(m)
  
})

test_that("2D Mapper can be partitioned",{
  m<-example_mapper_2d()
  m$distance    <- distance.mapper(m,method="euclidean")
  m$partitions  <- partition.mapper(m)
  # test partition structure
})


test_that("eccentricity with empty partitions doesn't crash mapper",{
  # high partition count low point count -> empty partitions
  l1  = lense(lense.eccentricity, lenseparam=1, partition_count=6, overlap = 0.5)
  d = circle_data(r=1,n=10)
  m <- mapper(dataset = d, 
                lenses=list(l1), 
                cluster_method="single", bin_count=10, normalize_data=FALSE)
  m$distance   <- distance.mapper(m,method="euclidean") # dist(scale(gm$d),method="euclidean", upper=FALSE)
  m$partitions <- partition.mapper(m)
  # previous this would crash! 
  
  expect_true(TRUE, "mapper crashed") # if we get here, mapper didn't crash
  
})

test_that("Grid data with 2d mapper,nodes have all have same size", {
  sizes = grid_2d_mapper(10,4) %>% mapper.run() %>% node_sizes
  # if nodes are same size, variance is zero
  expect_equal(var(sizes),0)
  
})

test_that("sequence 1-10 partitions are correct",{
  # create mapper on X=1..10, 5 partitions, 50% overlap
  m <- integer_mapper(10,5)
  m$partitions <- partition.mapper(m)
  expect_equal(m$partitions[[1]],c("1", "2", "3"))
  expect_equal(m$partitions[[2]],c("3", "4", "5"))
  expect_equal(m$partitions[[3]],c("4", "5", "6"))
  expect_equal(m$partitions[[4]],c("6", "7", "8"))
  expect_equal(m$partitions[[5]],c("7", "8", "9","10"))

})
test_that("mapper on integer sequence nodes have certain sizes ",{
  m <-  integer_mapper(10,5) 
  m$partitions <- partition.mapper(m) 
  sizes <- unlist(lapply(m$partitions,length))
  expect_equal(as.vector(sizes),c(3,3,3,3,4))
})

test_that("circle with 2 dims with 3 partitions has ",{
  d = circle_data(r=1, n=1000, randomize=FALSE)
  l1 = lense(lense.projection, "X", partition_count=3, overlap = 0.5) 
  l2 = lense(lense.projection, "Y", partition_count=3, overlap = 0.5) 
  m <- mapper(dataset = d, 
              lenses=list(l1,l2), 
              cluster_method="single", bin_count=10, normalize_data=TRUE)
  m <- mapper.run(m)
  for(nodeset in m$nodes) { expect_gt(length(nodeset),0)}
  expect_equal(length(m$nodes),8)
  
  # test of degree requires igraph library
  # degree of every node is 2
  gm = graph.mapper(m)
  expect_identical(as.vector(degree(gm)),rep(2,length(m$nodes)))
  
  
})  

