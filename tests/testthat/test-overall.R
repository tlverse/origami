library(origami)
library(data.table)
context("Overall Test")

r_major <- as.numeric(R.Version()$major)
r_minor <- as.numeric(R.Version()$minor)
if (r_major == 3 && r_minor < 6) {
  cvmse_result <- 13.32
} else {
  cvmse_result <- 14.11
}
set.seed(1)
data(mtcars)

# re-substitution MSE
r <- lm(mpg ~ ., data = mtcars)
simple_resub_MSE <- mean(resid(r)^2)

# function to calculate cross-validated squared error
cvlm <- function(fold) {
  train_data <- training(mtcars)
  valid_data <- validation(mtcars)

  r <- lm(mpg ~ ., data = train_data)
  preds <- predict(r, newdata = valid_data)
  list(coef = data.frame(t(coef(r))), SE = ((preds - valid_data$mpg)^2))
}

# replicate the resubstitution estimate
resub <- make_folds(mtcars, fold_fun = folds_resubstitution)[[1]]
resub_results <- cvlm(resub)
resub_MSE <- mean(resub_results$SE)
test_that("Resub MSE matches simple resub MSE", {
  expect_equal(resub_MSE, simple_resub_MSE, tolerance = 0.01)
})

# cross-validated estimate
folds <- make_folds(mtcars)
results <- cross_validate(cvlm, folds)
cv_MSE <- mean(results$SE)
test_that("CV MSE matches previous value", {
  expect_equal(cv_MSE, cvmse_result, tolerance = 0.01)
})
