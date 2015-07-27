
# origami SuperLearner produces a split sequential SuperLearner by default
#' @export
split_from_args <- function(osl_args) {
    fit <- do.call(origami_SuperLearner, osl_args)
}

# fit origami_SuperLearner once, to the full dataset and return either the
# FullSuperLearner or the SplitSequential SuperLearner for each fold
#' @export
split_to_full <- function(fit, osl_args = NULL) {
    # full sequntial uses the full fit for all the folds
    fit$foldFits <- replicate(length(folds), fit$fullFit, simplify = F)
    
    return(fit)
}

# generate nested folds for a given training set todo: allow user-specified
# nested fold generation
make_nest_folds <- function(Y, X, stratifyAY = FALSE, Anode, ...) {
    
    if (stratifyAY) {
        AYstrata <- sprintf("%s %s", X[, Anode], Y)
        folds <- make_folds(strata_ids = AYstrata)
    } else {
        folds <- make_folds(Y)
    }
    
    return(folds)
}

# fit origami_SuperLearner on a training subset
cv_nested_fit <- function(fold, osl_args, ...) {
    train_idx <- training()
    train_Y <- training(osl_args$Y)
    train_X <- training(osl_args$X)
    
    nest_folds <- make_nest_folds(train_Y, train_X)  #,...)
    
    osl_args$Y <- train_Y
    osl_args$X <- train_X
    osl_args$folds <- nest_folds
    
    fit <- do.call(origami_SuperLearner, osl_args)
    
    return(list(fit = fit$fullFit))
}

# fit origami_SuperLearner separately on the training subset of each fold
# (nested)
#' @export
split_to_nested <- function(fit, osl_args) {
    folds <- osl_args$folds
    nested_fits <- cross_validate(cv_nested_fit, folds = folds, Y = Y, X = X, osl_args = osl_args)
    fit$foldFits <- nested_fits$fit
    return(fit)
} 
