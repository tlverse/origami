# replicate foreach error collection
# function factory that generates wrapped version of functions
wrap_in_try <- function(fun, ...) {
    wrapped <- function(...)
    try({
        fun(...)
    }, silent = TRUE)
    
    return(wrapped)
}


################################################################################

#' Build a Fold Object for V-Fold CV
#'
#' For V-fold type cross-validation, take a fold vector and make fold object
#' for fold V.
#'
#' @family fold generation functions
#'
#' @param v - identifier of the fold in which observations fall for CV.
#' @param folds - vector of the fold status for each observation for CV.
#'
fold_from_foldvec <- function(v, folds) {
  training_set <- which(folds != v)
  validation_set <- which(folds == v)
  
  make_fold(v, training_set, validation_set)
}
