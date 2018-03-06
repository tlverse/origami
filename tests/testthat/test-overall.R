library(origami)
library(data.table)
context("Overall Test")

set.seed(1)

data(mtcars)

# resubstitution MSE
r <- lm(mpg ~ ., data = mtcars)
simple_resub_MSE <- mean(resid(r) ^ 2)

# function to calculate cross-validated squared error
cvlm <- function(fold) {
  train_data <- training(mtcars)
  valid_data <- validation(mtcars)

  r <- lm(mpg ~ ., data = train_data)
  preds <- predict(r, newdata = valid_data)
  list(coef = data.frame(t(coef(r))), SE = ((preds - valid_data$mpg) ^ 2))
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
  expect_equal(cv_MSE, 13.32, tolerance = 0.01)
})
