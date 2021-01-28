#' Wrap a Function in a Try Statement
#'
#' Function factory that generates versions of functions wrapped in \code{try}.
#'
#' @param fun A \code{function} to be wrapped in a \code{try} statement.
#' @param ... Additional arguments passed to the previous argument \code{fun}.
#'
#' @export
wrap_in_try <- function(fun, ...) {
  wrapped <- function(...) {
    try(
      {
        fun(...)
      },
      silent = TRUE
    )
  }
  return(wrapped)
}

################################################################################

#' Build a Fold Object from a Fold Vector
#'
#' For V-fold type cross-validation. This takes a fold vector (validation set
#' IDs) and builds a fold object for fold V.
#'
#' @family fold generation functions
#'
#' @param v An identifier of the fold in which observations fall for
#'  cross-validation.
#' @param folds A vector of the fold status for each observation for
#'  cross-validation.
#'
#' @export
fold_from_foldvec <- function(v, folds) {
  training_set <- which(folds != v)
  validation_set <- which(folds == v)
  make_fold(v, training_set, validation_set)
}

################################################################################

#' Build a Fold Vector from a Fold Object
#'
#' For V-fold type cross-validation. This takes a fold object and returns a fold
#' vector (containing the validation set IDs) for use with other tools like
#' \code{\link[glmnet]{cv.glmnet}}.
#'
#' @family fold generation functions
#'
#' @param folds A \code{fold} object as produced by \code{\link{make_folds}},
#'  from which a \code{numeric} vector of the validation set fold IDs are
#'  returned.
#'
#' @export
folds2foldvec <- function(folds) {
  fold_vec <- lapply(folds, function(fold) {
    validation()
  })
  len <- sapply(fold_vec, length)
  nums <- rep(seq_along(fold_vec), len)
  fold_index <- unlist(fold_vec)
  fold_id <- nums[order(fold_index)]
  return(fold_id)
}

################################################################################

#' @export
`%notin%` <- Negate(`%in%`)

#' @export
fold_from_foldvec_subgroup <- function(v, foldsdf, n) {
  validation_set <- foldsdf[,1][which(foldsdf[,2] == v)]
  training_set <- which(seq_len(n) %notin% validation_set)
  make_fold(v, training_set, validation_set)
}

#' @export
fold_from_foldvec_subgroup_stratoutcome <- function(v, foldsdf1, foldsdf2, n) {
  validation_set <- c(foldsdf1[,1][which(foldsdf1[,2] == v)], foldsdf2[,1][which(foldsdf2[,2] == v)])
  training_set <- which(seq_len(n) %notin% validation_set)
  make_fold(v, training_set, validation_set)
}
