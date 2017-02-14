context("Mapper Object")

### setup 
example_lense  <- function(){
  lense(lense.constant, lenseparam=NULL, partition_count=4, overlap = 0.5)
}

example_mapper <- function(){
  mapper(dataset = circle_data(), 
         lenses=list(example_lense()), 
         cluster_method="single", bin_count=10, normalize_data=TRUE)
}


### tests
test_that("Mapper is object", {
  
  m <- example_mapper()
  expect_is(m, "mapper")
  expect_equal(m$bin_count,10)

}
)