context("Mapper")

### first  helper_mapper.R file sets example_mapper

### tests
test_that("1d Mapper with iris data", {
  data(iris)
 d = iris

  l1 = lense(lense.projection, colnames(d)[1], partition_count=4, overlap = 0.5) 
  # l2 = lense(lense.projection, "Y", partition_count=3, overlap = 0.5) 
  m <- mapper(dataset = d, 
              lenses=list(l1), 
              cluster_method="single", bin_count=10, normalize_data=TRUE, 
              "selected_cols" =  names(d)[sapply(d,is.numeric),progressUpdater = NULL]
              )
  

  m <- mapper.run(m)
  expect_is(m,"mapper")
  
})