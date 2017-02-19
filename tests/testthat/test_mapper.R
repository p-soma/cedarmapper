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
  expect_equal(round(m$distance[1] - 0.1467897,7),0)
})

test_that("mapper partition calc works",{
  m <- example_mapper()
  m$distance    <- distance.mapper(m)
  m$partitions  <- partition.mapper(m)
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

test_that("2D Mapper can be partitioned",{
  m<-example_mapper_2d()
  m$distance    <- distance.mapper(m,method="euclidean")
  m$partitions  <- partition.mapper(m)
  # test partition structure
})



