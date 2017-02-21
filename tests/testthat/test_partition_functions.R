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
  
  # crash! 
  
})
