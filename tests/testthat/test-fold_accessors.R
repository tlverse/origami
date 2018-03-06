context("Fold Accessors")

test_that("Error on no fold defined", {
  expect_error(training())
})

fold <- "garbage fold"
test_that("Error on garbage fold", {
  expect_error(training())
})

training_fun <- get_from_fold("training_set")
test_that("Function factory works", {
  expect_equal(training, training_fun)
})
