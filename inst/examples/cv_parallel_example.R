###############################################################################
# This example explains how to use the cross_validate function with
# parallelization using the framework of the future package.
###############################################################################

suppressMessages(library(data.table))
library(future)
data(mtcars)
set.seed(1)

# make a lot of folds
folds <- make_folds(mtcars, fold_fun = folds_bootstrap, V = 1000)

# function to calculate cross-validated squared error for linear regression
cv_lm <- function(fold, data, reg_form) {
  # get name and index of outcome variable from regression formula
  out_var <- as.character(unlist(str_split(reg_form, " "))[1])
  out_var_ind <- as.numeric(which(colnames(data) == out_var))

  # split up data into training and validation sets
  train_data <- training(data)
  valid_data <- validation(data)

  # fit linear model on training set and predict on validation set
  mod <- lm(as.formula(reg_form), data = train_data)
  preds <- predict(mod, newdata = valid_data)

  # capture results to be returned as output
  out <- list(
    coef = data.frame(t(coef(mod))),
    SE = ((preds - valid_data[, out_var_ind])^2)
  )
  return(out)
}

plan(sequential)
time_seq <- system.time({
  results_seq <- cross_validate(
    cv_fun = cv_lm, folds = folds, data = mtcars,
    reg_form = "mpg ~ ."
  )
})

plan(multicore)
time_mc <- system.time({
  results_mc <- cross_validate(
    cv_fun = cv_lm, folds = folds, data = mtcars,
    reg_form = "mpg ~ ."
  )
})

if (availableCores() > 1) {
  time_mc["elapsed"] < 1.2 * time_seq["elapsed"]
}
