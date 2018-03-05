library(future)
context("Future Plan")

set.seed(1)

data(mtcars)
# make a lot of folds
folds <- make_folds(mtcars, fold_fun = folds_bootstrap, V = 1000)

# function to calculate cross-validated squared error
cvlm <- function(fold) {
  train_data <- training(mtcars)
  valid_data <- validation(mtcars)

  r <- lm(mpg ~ ., data = train_data)
  preds <- predict(r, newdata = valid_data)
  list(coef = data.frame(t(coef(r))), SE = ((preds - valid_data$mpg) ^ 2))
}

plan(sequential)
time_seq <- system.time({
  results_seq <- cross_validate(cvlm, folds)
})

plan(multicore)
time_mc <- system.time({
  results_mc <- cross_validate(cvlm, folds)
})

if (future::availableCores() > 1) {
  test_that("MC is not significantly slower than sequential", {
    skip_on_os("windows") # Windows doesn't support multicore
    expect_lt(time_mc["elapsed"], 1.2 * time_seq["elapsed"])
  })
}

######

# # verify globals are being transferred to other sessions
# a=4
# folds=make_folds(1000)
# return_a=function(fold){
#     list(a=a,dt=data.table(a=a))
# }

# plan(multisession,workers=2)
# results=cross_validate(return_a,folds)
# test_that("globals are available in worker sessions",expect_true(all(results$a==4)))
# test_that("globals are available in worker sessions",expect_is(results$dt,"data.table"))
