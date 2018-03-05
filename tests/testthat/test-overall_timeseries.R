library(forecast)
context("Overall Test for Time Series")

set.seed(1)
data(AirPassengers)

# simple fold
folds <- make_folds(
  AirPassengers, fold_fun = folds_rolling_origin,
  first_window = 36, validation_size = 24, gap = 0, batch = 1
)

fold <- folds[[1]]

# function to calculate cross-validated squared error
cvforecasts <- function(fold) {
  train_data <- training(AirPassengers)
  valid_data <- validation(AirPassengers)
  valid_size <- length(valid_data)

  train_ts <- ts(log10(train_data), frequency = 12)

  # borrowed from AirPassengers help
  arima_fit <- arima(
    train_ts, c(0, 1, 1),
    seasonal = list(
      order = c(0, 1, 1),
      period = 12
    )
  )
  raw_arima_pred <- predict(arima_fit, n.ahead = valid_size)
  arima_pred <- 10 ^ raw_arima_pred$pred
  arima_MSE <- mean((arima_pred - valid_data) ^ 2)

  # stl model
  stl_fit <- stlm(train_ts, s.window = 12)
  raw_stl_pred <- forecast(stl_fit, h = valid_size)
  stl_pred <- 10 ^ raw_stl_pred$mean
  stl_MSE <- mean((stl_pred - valid_data) ^ 2)

  list(mse = data.frame(fold = fold_index(), arima = arima_MSE, stl = stl_MSE))
}

mses <- cross_validate(cvforecasts, folds)$mse
mses_mean <- colMeans(mses[, c("arima", "stl")])

# Test we get the same result as in the vignette example:
test_that("CV MSE matches previous value", {
  expect_equal(mses_mean[[1]], 667.2478, tolerance = 0.01)
})

# Tests with gap and batch parameters:
folds <- make_folds(
  AirPassengers, fold_fun = folds_rolling_origin,
  first_window = 36, validation_size = 24, gap = 5, batch = 10
)

fold <- folds[[1]]
mses <- cross_validate(cvforecasts, folds)$mse
mses_mean <- colMeans(mses[, c("arima", "stl")])

test_that("CV MSE with gap and bacth matches previous value", {
  expect_equal(mses_mean[[1]], 6004.730, tolerance = 0.01)
})


# Tests with gap and batch parameters for rolling window CV:
folds <- make_folds(
  AirPassengers, fold_fun = folds_rolling_window,
  window_size = 36, validation_size = 24, gap = 5, batch = 2
)

fold <- folds[[1]]
mses <- cross_validate(cvforecasts, folds)$mse
mses_mean <- colMeans(mses[, c("arima", "stl")])

test_that("CV MSE with rolling window matches previous value", {
  expect_equal(mses_mean[[1]], 7580.455, tolerance = 0.01)
})
