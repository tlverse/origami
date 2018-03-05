context("Combine Results")

# test setup
raw_input <- list(a = list(1, 2, 3, 4))
output <- combine_results(raw_input, combiners = NULL, smart_combiners = F)

# invoke test
test_that("combine_results falls back by returning unprocessed input", {
  expect_equal(raw_input, output)
})

# test setup
nrows <- 10
ncols <- 10
nmats <- 5
nreps <- 10
md_array <- array(0, dim = c(nrows, ncols, nmats))
array_list <- replicate(nreps, md_array, simplify = F)
combined_array <- combiner_array(array_list)
expected_dims <- c(nrows * nreps, ncols, nmats)

# invoke test
test_that("combiner_array stacks on rows for multidimensional arrays", {
  expect_equal(dim(combined_array), expected_dims)
})
