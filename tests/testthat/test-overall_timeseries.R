context("Overall Test for Time Series")

if (require("forecast")) {
  set.seed(1)
  data(AirPassengers)

  # simple fold
  folds <- make_folds(
    AirPassengers,
    fold_fun = folds_rolling_origin,
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
    arima_pred <- 10^raw_arima_pred$pred
    arima_MSE <- mean((arima_pred - valid_data)^2)

    # stl model
    stl_fit <- stlm(train_ts, s.window = 12)
    raw_stl_pred <- forecast(stl_fit, h = valid_size)
    stl_pred <- 10^raw_stl_pred$mean
    stl_MSE <- mean((stl_pred - valid_data)^2)

    list(mse = data.frame(
      fold = fold_index(), arima = arima_MSE,
      stl = stl_MSE
    ))
  }

  mses <- cross_validate(cvforecasts, folds)$mse
  mses_mean <- colMeans(mses[, c("arima", "stl")])

  # Test we get the same result as in the vignette example:
  test_that("CV-MSE matches previous value", {
    expect_equal(mses_mean[[1]], 667.2478, tolerance = 0.01)
  })

  # Tests with gap and batch parameters:
  folds <- make_folds(
    AirPassengers,
    fold_fun = folds_rolling_origin,
    first_window = 36, validation_size = 24, gap = 5, batch = 10
  )

  fold <- folds[[1]]
  mses <- cross_validate(cvforecasts, folds)$mse
  mses_mean <- colMeans(mses[, c("arima", "stl")])

  test_that("CV-MSE with gap and batch matches previous value", {
    expect_equal(mses_mean[[1]], 6004.730, tolerance = 0.01)
  })


  # Tests with gap and batch parameters for rolling window CV:
  folds <- make_folds(
    AirPassengers,
    fold_fun = folds_rolling_window,
    window_size = 36, validation_size = 24, gap = 5, batch = 2
  )

  fold <- folds[[1]]
  mses <- cross_validate(cvforecasts, folds)$mse
  mses_mean <- colMeans(mses[, c("arima", "stl")])

  test_that("CV-MSE with rolling window matches previous value", {
    expect_equal(mses_mean[[1]], 7580.455, tolerance = 0.01)
  })

  #############################################################################
  # Test multiple time-series functionality
  n_id <- 100
  test_data <- data.table(id = seq_len(n_id))
  test_data <- test_data[, list(t = seq_len(floor(runif(1) * 50))),
    by = list(id)
  ]
  test_data[, X := rnorm(.N)]

  ### Independent sample example
  folds <- make_folds(test_data,
    fold_fun = folds_rolling_origin_pooled,
    id = test_data$id,
    t = max(test_data$t),
    time = test_data$t, first_window = 4,
    validation_size = 4, gap = 0, batch = 2
  )

  test_that("Size of the first fold of rolling origin pooled CV", {
    expect_equal(length(folds[[1]]$training_set), sum(test_data$t <= 4))
  })

  folds <- make_folds(test_data,
    fold_fun = folds_rolling_window_pooled,
    id = test_data$id,
    t = max(test_data$t),
    time = test_data$t,
    window_size = 4,
    validation_size = 4, gap = 0, batch = 2
  )
  test_that("Size of the first fold of rolling window pooled CV", {
    expect_equal(length(folds[[1]]$training_set), sum(test_data$t <= 4))
  })

  ### Independent sample example:
  #   not the same number of time points
  #   id based
  test_data_id <- data.table(melt(data.table(AirPassengers),
    measure.vars = "AirPassengers"
  ),
  id = c(rep(1, 60), rep(2, 84))
  )

  folds_id1 <- make_folds(test_data_id,
    fold_fun = folds_rolling_window_pooled,
    t = 60, window_size = 5, id = 1, time = seq(1:60),
    validation_size = 5, gap = 0, batch = 20
  )
  folds_id2 <- make_folds(test_data_id,
    fold_fun = folds_rolling_window_pooled,
    t = 84, window_size = 5, id = 2, time = as.matrix(seq(1:84)),
    validation_size = 7, gap = 0, batch = 20
  )
  test_that("Size of CVs rolling window pooled CV, id 2", {
    expect_equal(length(folds_id2), 4)
  })
  test_that("Size of CVs rolling window pooled CV, id 1", {
    expect_equal(length(folds_id1), 3)
  })

  folds_id1 <- make_folds(test_data_id,
    fold_fun = folds_rolling_origin_pooled,
    t = 60, first_window = 10, id = 1, time = seq(1:60),
    validation_size = 5, gap = 0, batch = 20
  )
  folds_id2 <- make_folds(test_data_id,
    fold_fun = folds_rolling_origin_pooled,
    t = 84, first_window = 10, id = 2, time = seq(1:84),
    validation_size = 5, gap = 0, batch = 20
  )
  test_that("Size of CVs rolling origin pooled CV, id 2", {
    expect_equal(length(folds_id2), 4)
  })
  test_that("Size of CVs rolling origin pooled CV, id 1", {
    expect_equal(length(folds_id1), 3)
  })

  ### Dependent samples example
  folds <- make_folds(test_data,
    fold_fun = folds_vfold_rolling_origin_pooled,
    t = 12, first_window = 6, V = 5,
    validation_size = 2, gap = 0, batch = 2
  )
  test_that("Dimension of folds for the V-fold rolling origin pooled CV", {
    expect_equal(length(folds), 15)
  })

  folds <- make_folds(test_data,
    fold_fun = folds_vfold_rolling_window_pooled,
    t = 12, window_size = 6, V = 5,
    validation_size = 2, gap = 0, batch = 3
  )
  test_that("Dimension of folds for the V-fold rolling window pooled CV", {
    expect_equal(length(folds), 10)
  })

  ### Example with multivariate time series with variations in what's observed:
  # for each id, vary time measured and number of observations
  value <- AirPassengers
  time <- c(1:48, c(1, 5:51), c(1:10, 31:68))
  id <- rep(1:3, each = 48)
  dat <- data.table(id, time, value)

  folds <- make_folds(dat,
    t = 40, id = dat$id, time = dat$time,
    fold_fun = folds_rolling_origin_pooled,
    first_window = 10, validation_size = 10, gap = 0, batch = 10
  )

  # rows which should not be included in any folds
  bad_index1 <- which(dat$time > 40)
  test_that("Time included in folds does not exceed t", {
    expect_false(any(bad_index1 %in% unlist(folds)))
  })

  # no rows from id 3 should be included in fold 2 validation set
  # since time 21-30 not observed for id 3, and fold 2 validation is times 21-30
  id3 <- which(dat$id == 3)
  test_that("Validation folds respect differences in time for each id", {
    expect_false(any(id3 %in% folds[[2]]$validation_set))
  })

  # only 7 rows from id 2 should be included in fold 1 training set
  # since times 2-4 not observed for id 2, and fold 1 training is times 1-10
  id2_fold1 <- which(dat$id == 2 & dat$time <= 10)
  test_that("Training folds respect differences in time for each id", {
    expect_equal(length(id2_fold1), 7)
  })
}
