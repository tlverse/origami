#' Cross-Validation Schemes
#'
#' These functions represent different cross-validation schemes that can be used
#' with origami. They should be used as options for the \code{fold_fun} argument
#' to the \code{\link{make_folds}} function in this package.
#' \code{\link{make_folds}} will call the requested function specify \code{n},
#' based on its arguments, and pass any remaining arguments (e.g. \code{V} or
#' \code{pvalidation}) on.
#'
#' @family fold generation functions
#'
#' @param n (integer) - number of observations.
#' @param V (integer) - number of folds.
#' @param pvalidation (double) - proportion of observation to be in validation
#'  fold.
#' @param first_window (integer) - number of observations in the first training
#'  sample.
#' @param window_size (integer) - number of observations in each training
#'  sample.
#' @param validation_size (integer) - number of points in the validation
#'  samples; should be equal to the largest forecast horizon.
#' @param gap (integer) - number of points not included in the training or
#'  validation samples; Default is 0.
#' @param batch (integer) - Increases the number of time-points added to the
#'  training set each CV iteration. Applicable for larger time-series. Default
#'  is 1.
#' @param t (integer) - Number of time points per time-series sample.
#'
#' @return A list of Folds.
#' @name fold_funs
NULL

################################################################################

#' @rdname fold_funs
#' @export
folds_vfold <- function(n, V = 10) {
  if (n <= V) {
    warning("n <= V so using leave-one-out CV")
    return(folds_loo(n))
  }
  folds <- rep(seq_len(V), length = n)

  # shuffle folds
  folds <- sample(folds)

  # generate fold vectors
  folds <- lapply(1:V, fold_from_foldvec, folds)
  return(folds)
}

################################################################################

#' @rdname fold_funs
#' @export
folds_resubstitution <- function(n) {
  list(make_fold(1L, seq_len(n), seq_len(n)))
}

################################################################################

#' @rdname fold_funs
#' @export
folds_loo <- function(n) {
  # folds are trivial here
  folds <- seq_len(n)

  # generate fold vectors
  folds <- lapply(folds, fold_from_foldvec, folds)
  return(folds)
}

################################################################################
#' @rdname fold_funs
#' @export
#'
folds_montecarlo <- function(n, V = 1000, pvalidation = 0.2) {
  stopifnot(pvalidation > 0 && pvalidation < 1)

  # calculate training sample size
  ntrain <- round((1 - pvalidation) * n)
  stopifnot(ntrain > 1)

  folds <- lapply(seq_len(V), fold_montecarlo, n, ntrain, replace = F)
  return(folds)
}

# make monte carlo type folds
fold_montecarlo <- function(v, n, ntrain, replace) {
  training_set <- sample(n, size = ntrain, replace = replace)
  validation_set <- setdiff(seq_len(n), training_set)
  make_fold(v, training_set, validation_set)
}

################################################################################

#' @rdname fold_funs
#' @export
folds_bootstrap <- function(n, V = 1000) {
  folds <- lapply(seq_len(V), fold_montecarlo, n, n, replace = T)
  return(folds)
}

################################################################################

#' @rdname fold_funs
#' @export
folds_rolling_origin <- function(n, first_window, validation_size, gap = 0, batch = 1) {
  last_window <- n - (validation_size + gap)
  origins <- seq.int(first_window, last_window, by = batch)

  folds <- lapply(seq_along(origins), function(i) {
    origin <- origins[i]
    make_fold(v = i, training_set = 1:origin, validation_set = origin + gap +
      (1:validation_size))
  })
  return(folds)
}

################################################################################

#' @rdname fold_funs
#' @export
folds_rolling_window <- function(n, window_size, validation_size, gap = 0, batch = 1) {
  last_window <- n - (validation_size + gap)
  origins <- seq.int(window_size, last_window, by = batch)

  folds <- lapply(seq_along(origins), function(i) {
    origin <- origins[i]
    make_fold(
      v = i, training_set = (1:window_size) + (i * batch - batch),
      validation_set = origin + gap + (1:validation_size)
    )
  })
  return(folds)
}

################################################################################

#' @rdname fold_funs
#' @export
folds_rolling_origin_pooled <- function(n, t, first_window, validation_size,
                                        gap = 0, batch = 1) {
  
  message(paste("Processing", n/t, "samples with", t, "time points."))
  
  #Index the observations
  dat <- cbind.data.frame(index=seq(n),time=rep(seq(t),n/t),id=rep(seq(n/t), each=t))
  ids <- unique(dat$id)
  
  # establish rolling origin forecast for time-series cross-validation
  rolling_origin_skeleton <- folds_rolling_origin(t, first_window,
                                                  validation_size, gap, batch)
  
  folds_rolling_origin <- lapply(rolling_origin_skeleton, function(h){
    train_indices <- lapply(ids, function(i){
      train <- dat[dat$id == i, ]
      train[h$training_set, ]$index
    })
    val_indices <- lapply(ids, function(j){
      val <- dat[dat$id == j, ]
      val[h$validation_set, ]$index
    })
    make_fold(v=h$v, training_set=unlist(train_indices), 
              validation_set=unlist(val_indices))
  })
  return(folds_rolling_origin)
}

#' @rdname fold_funs
#' @export
folds_rolling_window_pooled <- function(n, t, window_size, validation_size,
                                        gap = 0, batch = 1) {
  
  message(paste("Processing", n/t, "samples with", t, "time points."))
  
  #Index the observations
  dat <- cbind.data.frame(index=seq(n),time=rep(seq(t),n/t),id=rep(seq(n/t), each=t))
  ids <- unique(dat$id)
  
  # establish rolling window forecast for time-series cross-validation
  rolling_window_skeleton <- folds_rolling_window(t, window_size,
                                                  validation_size, gap, batch)
  
  folds_rolling_window <- lapply(rolling_window_skeleton, function(h){
    train_indices <- lapply(ids, function(i){
      train <- dat[dat$id == i, ]
      train[h$training_set, ]$index
    })
    val_indices <- lapply(ids, function(j){
      val <- dat[dat$id == j, ]
      val[h$validation_set, ]$index
    })
    make_fold(v=h$v, training_set=unlist(train_indices), 
              validation_set=unlist(val_indices))
  })
  return(folds_rolling_window)
}