context("Lenses")


test_that("can make a lense object", {
  lensefun = lense.constant
  l <- lense(lensefun, lenseparam=NULL, partition_count=4, overlap = 0.5)
  expect_is(l, "lense")
  
})

test_that("constant lense works", {
  L  = lense(lense.constant, lenseparam=NULL, partition_count=4, overlap = 0.5)
  
  d = as.data.frame( rnorm(100))
  lvalues <- L$lensefun(d, L$lenseparam)
  expect_equal(lvalues,rep(0,times=nrow(d)))
  
  d = circle_data()
  lvalues <- L$lensefun(d, L$lenseparam)
  expect_equal(lvalues,rep(0,times=nrow(d)))
})

test_that("eccentricity of circle data is constant", {
  L  = lense(lense.eccentricity, lenseparam=1, partition_count=4, overlap = 0.5)
  
  d = circle_data()
  lvalues <- L$lensefun(d, L$lenseparam)
  expect_equal(round(var(lvalues)),0)
  # same number of values from function as rows of data
  expect_equal(nrow(d),lenght(lvalues))
})
