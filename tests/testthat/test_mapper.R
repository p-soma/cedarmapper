context("Mapper Object")

### helper functions are in helper_mapper.R file
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

