# This example explains how to use the cross_validate function with
# parallelization using the framework of the future package.

library(origami)
library(data.table)
library(future)

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
    list(coef = data.frame(t(coef(r))), SE = ((preds - valid_data$mpg)^2))
}

plan(sequential)
time_seq <- system.time({
    results_seq <- cross_validate(cvlm, folds)
})

plan(multicore)
time_mc <- system.time({
    results_mc <- cross_validate(cvlm, folds)
})

if(availableCores() > 1) {
    time_mc["elapsed"] < 1.2 * time_seq["elapsed"]
}
