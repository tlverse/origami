#' Fold
#'
#' Functions to make a fold. Current representation is a simple \code{list}.
#'
#' @param v An integer index of folds in the larger scheme.
#' @param training_set An integer vector of indexes corresponding to the
#'  training set.
#' @param validation_set An integer vector of indexes corresponding to the
#'  validation set.
#' @return A list containing these elements.
#'
#' @seealso \code{\link{fold_helpers}}
#'
#' @export
make_fold <- function(v, training_set, validation_set) {
  fold <- list(
    v = v,
    training_set = training_set,
    validation_set = validation_set
  )
  class(fold) <- "fold"
  return(fold)
}

# function factory for different fold-based indexing functions
get_from_fold <- function(component) {
  # function to index an object based on the training set,
  # validation set, or index of a fold
  function(x = NULL, fold = NULL) {
    # if fold isn't specified, attempt to pull from the calling
    # environment
    if (is.null(fold)) {
      if (!exists("fold", envir = parent.frame())) {
        stop("no fold specified, and fold not defined in calling function")
      }
      fold <- get("fold", envir = parent.frame())
    }

    if (class(fold) != "fold") {
      stop("invalid fold")
    }

    index <- fold[[component]]

    if (is.null(x)) {
      # if no x, return indexes
      return(index)
    } else if (is.null(dim(x))) {
      # if no dim, index on only dimension
      return(x[index])
    } else {
      # if dim, index on first dimension
      return(x[index, , drop = FALSE])
    }
  }
}

################################################################################

#' Fold Helpers
#'
#' Accessors and indexers for the different parts of a fold.
#'
#' @param x an object to be indexed by a training set, validation set, or fold
#'  index. If missing, the index itself will be returned.
#' @param fold Fold; the fold used to do the indexing. If missing, \code{fold}
#'  will be pulled from the calling environment, if available.
#'
#' @return The elements of \code{x} corresponding to the indexes, or the
#'  indexes themselves if \code{x} is missing.
#'
#' @rdname fold_helpers
#'
#' @name fold_helpers
#'
#' @seealso \code{\link{make_fold}}
NULL

#' @rdname fold_helpers
#' @export
training <- get_from_fold("training_set")

#' @rdname fold_helpers
#' @export
validation <- get_from_fold("validation_set")

#' @rdname fold_helpers
#' @export
fold_index <- get_from_fold("v")
