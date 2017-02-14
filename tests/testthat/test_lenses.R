context("Lenses")

test_that("constant lense works",{
  
})

test_that("lenses are a thing", {
  lensefun = lense.constant
  l <- lense(lensefun, lenseparam=NULL, partition_count=4, overlap = 0.5)
  expect_is(l, "lense")
  
})
