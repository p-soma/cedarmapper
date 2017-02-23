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
              "selected_cols" =  names(d)[sapply(d,is.numeric)])
  

  m <- mapper.run(m)
  expect_is(m,"mapper")
  
})

test_that("mapper.numeric_cols returns numeric columns", {
  d  = data.frame("x"=1:5,"y"=c("a","b","c","d","e"))
  m <- mapper(dataset = d, lenses = list(l1 = lense(lense.constant) ))
  expect_equal(mapper.numeric_cols(m),"x")
})

test_that("mapper in iris datareturns numeric columns", {
  d = iris
  l1 <- lense(lense.projection, colnames(d)[1], partition_count=4, overlap = 0.5)
  m  <- mapper(dataset = d, lenses = list(l1 = lense(lense.constant) ))
  expect_equal(mapper.numeric_cols(m), c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ))
  d <- m$d[mapper.numeric_cols(m)]
  expect_equal(length(d),4)  # four columns remain
  
  m <- mapper.run(m)
  
})

test_that("can convert factor to binary columns",{
  data("chemdiab")
  binary_df <- column2binary(chemdiab,"cc")
  expect_is(binary_df, "data.frame")
  expect_equal(nrow(binary_df), nrow(chemdiab))
  expect_equal(ncol(binary_df), 3)
  # note this last test may change if we change how the names are assigned
  expect_equal(names(binary_df)[1], "cc-Chemical_Diabetic" )
  
})
  