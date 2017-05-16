context("Mapper Object")

### first  helper_mapper.R file sets example_mapper

d = circle_data()
### tests
test_that("Mapper is object", {
  
  m <- example_mapper()
  expect_is(m, "mapper")
  expect_equal(m$bin_count,10)
})

test_that("mapper distance calc",{
  m <- example_mapper()
  m$distance <-  distance.mapper(m,method="euclidean")
  expect_equal( class(m$distance), "dist")
  expect_equal(attr(m$distance,"method"), "euclidean")
  # not sure this will work, depends on circle data being used
  # to 6 decimal places the same 

  
})

test_that("pipeline works for 1d projection lense on circle data",{
  
  # small helper function
  clusters_in_partition<-function(i){ length(unique(m$clusters[[i]])) }
  
  m <- circle_mapper(npoints=60,partition_count=4, overlap = 0.5)
  m$normalize_data <- TRUE
  m$cluster_method = 'single'
  
  m$distance <-  distance.mapper(m,method="euclidean")
  m$partitions <- partition.mapper(m)
  m$clusters   <- clusters.mapper(m, shinyProgressFunction=NULL ) 
  m$nodes      <- nodes.mapper(m)
  
  expect_equal(round(m$distance[1] - 0.1467897,7),0)
  expect_equal(length(m$distance),(60*(60-1))/2)
  expect_equal(length(m$partitions[[4]]), 27)
  
  expect_equal( clusters_in_partition(1), 1)
  expect_equal( clusters_in_partition(2), 2)
  expect_equal( clusters_in_partition(3), 2)
  expect_equal( clusters_in_partition(4), 1)
  
  expect_equal(length(m$nodes), 6)
  # spot check node # 6
  expect_equal(length(m$nodes[[6]]),27)
  
  # TODO test edge count
  
})

test_that("mapper.run function works",{
  m <- circle_mapper(npoints=60,partition_count=4, overlap = 0.5)
  m <- mapper.run(m)
  expect_equal(length(m$nodes), 6)
  # spot check node # 6
  expect_equal(length(m$nodes[[6]]),27)
  
})

test_that("2D Mapper can be created",{
  m<-example_mapper_2d()
  expect_is(m, "mapper")
  
  for(d in 1:2) {
    L <- m$lenses[[d]]
    expect_is(L,"lense") 
  }
  
  expect_equal(length(m$lenses),2)
  expect_equal(mapper.dimensions(m),2)

})


test_that("Square with 2 dims with 3 partitions has 9 nodes",{
    d = square_data(n=1000,seed="10")
    l1 = lense(lense.projection, "X", partition_count=3, overlap = 0.5) 
    l2 = lense(lense.projection, "Y", partition_count=3, overlap = 0.5) 
    m <- mapper(dataset = d, 
           lenses=list(l1,l2), 
           cluster_method="single", bin_count=10, normalize_data=TRUE)
    m <- mapper.run(m)
    expect_equal(length(m$nodes), 9)
    
  })  

