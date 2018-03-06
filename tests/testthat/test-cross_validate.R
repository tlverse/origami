context("Main Cross-Validation Method")

###########################

# test error handling for failed folds
cv_sometimes_errors <- function(fold) {
  index <- fold_index()
  if (index == 7) {
    stop("7 makes me sad")
  }


  return(list(a = "certainly not 7"))
}


folds <- make_folds(1000)

results <- cross_validate(cv_sometimes_errors, folds)

test_that("Errors are put in special error vector", {
  expect_length(results$errors$error, 1)
})


###########################

# test error handling when all folds fail

cv_always_errors <- function(fold) {
  stop("I always produce an error")
}


folds <- make_folds(1000)

suppressWarnings({
  results <- cross_validate(cv_always_errors, folds)
})

test_that("cross_validate handles the case where all folds produce an error", {
  expect_equal(names(results), "errors")
})
