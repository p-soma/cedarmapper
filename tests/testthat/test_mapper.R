context("Mapper Object")

### helper functions are in helper_mapper.R file

### tests
test_that("Mapper is object", {
  
  m <- example_mapper()
  expect_is(m, "mapper")
  expect_equal(m$bin_count,10)

}
)