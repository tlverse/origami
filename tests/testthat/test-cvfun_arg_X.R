context("cv_fun handles argument 'x' but not 'X' due to lapply conventions.")

# returns 'nrow' of input x
nrow_x_1 <- function(fold, x) {
  train_x <- training(x)
  valid_x <- validation(x)
  out <- nrow(valid_x)
  return(list(nrow_valid = out))
}

# returns 'nrow' of input X (note: 'X' is reserved in lapply / future_lapply)
nrow_X_2 <- function(fold, X) {
  train_X <- training(X)
  valid_X <- validation(X)
  out <- nrow(valid_X)
  return(list(nrow_valid = out))
}

# create fold object for using origami::cross_validate
folds <- make_folds(mtcars)

# cross_validate behaves as expected with cv_fun naming input argument 'x'
test_that("cross_validate handles cv_fun with input argument 'x'", {
  expect_equal(
    cross_validate(
      cv_fun = nrow_x_1, folds = folds,
      x = mtcars
    )$nrow_valid,
    c(4, 4, 3, 3, 3, 3, 3, 3, 3, 3)
  )
})

# cross_validate fails with cv_fun naming input argument 'X'
test_that("cross_validate rejects cv_fun with input argument 'X'", {
  expect_error(
    cross_validate(cv_fun = nrow_X_2, folds = folds, X = mtcars),
    "cv_fun names argument 'X'", ignore.case = TRUE
  )
})
