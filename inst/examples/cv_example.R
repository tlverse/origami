data(mtcars)

# resubstitution MSE
r <- lm(mpg ~ ., data = mtcars)
mean(resid(r)^2)

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
mean(resub_results$SE)

# cross-validated estimate
folds <- make_folds(mtcars)
results <- cross_validate(cvlm, folds)
mean(results$SE)
