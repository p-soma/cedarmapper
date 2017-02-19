context("Parititioning")

# see helpder_cedar.R for object creations functions

projectionlense <- function(varname){
  lense(lense.projection, varname, partition_count=5, overlap = 0.5)
}

integer_data <- function(n){
  data.frame("X" = 1:n)
}

decimal_data <- function(n){
  data.frame("X" = sapply(c(0,1:n),FUN=function(i){i/n}))
}

test_that("partition calculations", {
  expect_equal(10, 10)
  m <- mapper(d=data.frame("X"=c(1:13)),
                lenses = list(projectionlense("X")),
                cluster_method="single", bin_count=10, normalize_data=FALSE)
  m$distance <- distance.mapper(m)
  m$lenses[[1]] <- mapper.lense.calculate(m,1)
  L <- m$lenses[[1]]
  expect_equal(partition_start(L,1),1)
  expect_equal(partition_start(L,2),3)
  
  partition_indexes = 1:L$n
  # this gives the partition starting vlues
  all_partition_starting_values <- sapply(1:L$n, partition_start, L)
  expect_equal ( sapply( all_partition_starting_values ,get_partition_index,L), partition_indexes)
  
})
