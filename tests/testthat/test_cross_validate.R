library(origami)
library(data.table)
context("Main Cross-Validation Method")

############################ test error handling for failed folds

cv_errors <- function(fold) {
    index <- fold_index()
    if (index == 7) {
        stop("7 makes me sad")
    }
    
    
    return(list(a = "certainly not 7"))
}


folds <- make_folds(1000)

results <- cross_validate(cv_errors, folds)

test_that("Errors are put in special error vector", expect_length(results$errors$error, 
    1))
