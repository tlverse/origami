library(origami)
library(data.table)
context("Future Plan")


set.seed(1)

data(mtcars)
# make a lot of folds
folds <- make_folds(mtcars, fold_fun = "bootstrap", V = 1000)


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

if(future::availableCores() > 1) {
    test_that("MC is not significantly slower than sequential",{
        #Windows doesn't support multicore
        skip_on_os("windows")
        expect_lt(time_mc["elapsed"], 1.1*time_seq["elapsed"])
      })
}
