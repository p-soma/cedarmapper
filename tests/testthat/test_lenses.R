context("Lenses")


test_that("can make a lense object", {
  lensefun = lense.constant
  partition_count=4
  overlap = 0.5
  l <- lense(lensefun, lenseparam=NULL, partition_count, overlap)
  expect_is(l, "lense")
  expect_equal(l$o,overlap)
  expect_equal(l$n,partition_count)

})

test_that("1D Lense can be calculated",{
  lenseparam <- "X"
  L <- lense(lense.projection, lenseparam, partition_count=4, overlap = 0.5) 
  m <- mapper(dataset = circle_data(), 
         lenses=list(L), 
         cluster_method="single", bin_count=10, normalize_data=TRUE)
  m$distance = dist(m$d)
  L = mapper.lense.calculate(m)
  expect_is(L,"lense")
  
#  L$p0 <- min(L$values)
#  total_length = max(L$values) - L$p0
#  L$pl <- total_length/(L$n - ((L$n-1)*lense$o))

    expect_equal(L$p0,min(m$d$X))
    lense_partition_length_circle_data = 0.8
    expect_equal(L$pl,lense_partition_length_circle_data)  
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
  expect_equal(nrow(d),length(lvalues))

})

test_that("eccentricity on uniform circle doesn't crash mapper",{
  l1  = lense(lense.eccentricity, lenseparam=1, partition_count=5, overlap = 0.5)
  eccentricity_on_circle<- function(n=100){
      d = circle_data(r=1,n=1000)
      m <- mapper(dataset = d, 
              lenses=list(l1), 
              cluster_method="single", bin_count=10, normalize_data=FALSE)
      mapper.run(m)
  }
  
  ec <- eccentricity_on_circle(n=100)
  expect_equal(length(ec$nodes),2)

})

