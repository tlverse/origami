#' Cross-Validation Schemes
#'
#' These functions represent different cross-validation schemes that can be used
#' with \pkg{origami}. They should be used as options for the \code{fold_fun}
#' argument to \code{\link{make_folds}}, which will call the requested function
#' specify \code{n}, based on its arguments, and pass any remaining arguments
#' (e.g. \code{V} or \code{pvalidation}) on.
#'
#' @family fold generation functions
#'
#' @param n An integer indicating the number of observations.
#' @param V An integer indicating the number of folds.
#' @param pvalidation A \code{numeric} indicating the proportion of observation
#'  to be placed in the validation fold.
#' @param first_window An integer indicating the number of observations in the
#'  first training sample.
#' @param window_size An integer indicating the number of observations in each
#'  training sample.
#' @param validation_size An integer indicating the number of points in the
#'  validation samples; should be equal to the largest forecast horizon.
#' @param gap An integer indicating the number of points not included in the
#'  training or validation samples. The default is zero.
#' @param batch An integer indicating increases in the number of time points
#'  added to the training set in each iteration of cross-validation. Applicable
#'  for larger time-series. The default is one.
#' @param t An integer indicating the number of time points per time-series
#'  sample.
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
folds_rolling_origin <- function(n, first_window, validation_size, gap = 0,
                                 batch = 1) {
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
folds_rolling_window <- function(n, window_size, validation_size, gap = 0,
                                 batch = 1) {
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

### Samples independent
#' @rdname fold_funs
#' @export
folds_rolling_origin_pooled <- function(n, t, first_window, validation_size,
                                        gap = 0, batch = 1) {
  message(paste("Processing", n / t, "samples with", t, "time points."))

  # Index the observations
  dat <- cbind.data.frame(
    index = seq(n), time = rep(seq(t), n / t),
    id = rep(seq(n / t), each = t)
  )
  ids <- unique(dat$id)

  # establish rolling origin forecast for time-series cross-validation
  rolling_origin_skeleton <- folds_rolling_origin(
    t, first_window,
    validation_size, gap, batch
  )

  folds_rolling_origin <- lapply(rolling_origin_skeleton, function(h) {
    train_indices <- lapply(ids, function(i) {
      train <- dat[dat$id == i, ]
      train[h$training_set, ]$index
    })
    val_indices <- lapply(ids, function(j) {
      val <- dat[dat$id == j, ]
      val[h$validation_set, ]$index
    })
    make_fold(
      v = h$v, training_set = unlist(train_indices),
      validation_set = unlist(val_indices)
    )
  })
  return(folds_rolling_origin)
}

#' @rdname fold_funs
#' @export
folds_rolling_window_pooled <- function(n, t, window_size, validation_size,
                                        gap = 0, batch = 1) {
  message(paste("Processing", n / t, "samples with", t, "time points."))

  # Index the observations
  dat <- cbind.data.frame(
    index = seq(n), time = rep(seq(t), n / t),
    id = rep(seq(n / t), each = t)
  )
  ids <- unique(dat$id)

  # establish rolling window forecast for time-series cross-validation
  rolling_window_skeleton <- folds_rolling_window(
    t, window_size,
    validation_size, gap, batch
  )

  folds_rolling_window <- lapply(rolling_window_skeleton, function(h) {
    train_indices <- lapply(ids, function(i) {
      train <- dat[dat$id == i, ]
      train[h$training_set, ]$index
    })
    val_indices <- lapply(ids, function(j) {
      val <- dat[dat$id == j, ]
      val[h$validation_set, ]$index
    })
    make_fold(
      v = h$v, training_set = unlist(train_indices),
      validation_set = unlist(val_indices)
    )
  })
  return(folds_rolling_window)
}

### Samples dependent
#' @rdname fold_funs
#' @export

folds_vfold_rolling_origin_pooled <- function(n, t, V = 10, first_window,
                                              validation_size, gap = 0,
                                              batch = 1) {
  message(paste("Processing", n / t, "samples with", t, "time points."))

  # Index the observations
  dat <- cbind.data.frame(
    index = seq(n), time = rep(seq(t), n / t),
    id = rep(seq(n / t), each = t)
  )
  ids <- unique(dat$id)

  # establish V folds for cross-validating ids
  Vfold_allocation <- sample(rep(seq_len(V), length = length(ids)))
  Vfolds_skeleton <- lapply(1:V, fold_from_foldvec, Vfold_allocation)

  # establish rolling origin forecast for time-series cross-validation
  rolling_origin_skeleton <- folds_rolling_origin(
    t, first_window,
    validation_size, gap, batch
  )

  # Put it all together: gives V-fold and rolling structure
  Vfolds_rolling_origin_pooled <- lapply(Vfolds_skeleton, function(g) {

    # Samples
    training_ids <- g$training_set
    validation_ids <- g$validation_set

    # Time
    folds_rolling_origin <- lapply(rolling_origin_skeleton, function(h) {
      train <- dat[
        (dat$time %in% h$training_set & dat$id %in% training_ids),
        "index"
      ]
      val <- dat[
        (dat$time %in% h$validation_set & dat$id %in% validation_ids),
        "index"
      ]
      make_fold(
        v = (length(rolling_origin_skeleton) * (g$v - 1) + h$v),
        training_set = train, validation_set = val
      )
    })
  })

  # flatten nested structure
  Vfolds_rolling_origin_pooled <- unlist(Vfolds_rolling_origin_pooled,
    recursive = FALSE
  )
  return(Vfolds_rolling_origin_pooled)
}

#' @rdname fold_funs
#' @export
folds_vfold_rolling_window_pooled <- function(n, t, V = 10, window_size,
                                              validation_size, gap = 0,
                                              batch = 1) {
  message(paste("Processing", n / t, "samples with", t, "time points."))

  # Index the observations
  dat <- cbind.data.frame(
    index = seq(n), time = rep(seq(t), n / t),
    id = rep(seq(n / t), each = t)
  )
  ids <- unique(dat$id)

  # establish V folds for cross-validating ids
  Vfold_allocation <- sample(rep(seq_len(V), length = length(ids)))
  Vfolds_skeleton <- lapply(1:V, fold_from_foldvec, Vfold_allocation)

  # establish rolling origin forecast for time-series cross-validation
  rolling_window_skeleton <- folds_rolling_window(
    t, window_size,
    validation_size, gap, batch
  )

  # Put it all together: gives V-fold and rolling structure
  Vfolds_rolling_window_pooled <- lapply(Vfolds_skeleton, function(g) {

    # Samples
    training_ids <- g$training_set
    validation_ids <- g$validation_set

    # Time
    folds_rolling_window <- lapply(rolling_window_skeleton, function(h) {
      train <- dat[
        (dat$time %in% h$training_set & dat$id %in% training_ids),
        "index"
      ]
      val <- dat[
        (dat$time %in% h$validation_set & dat$id %in% validation_ids),
        "index"
      ]
      make_fold(
        v = (length(rolling_window_skeleton) * (g$v - 1) + h$v),
        training_set = train,
        validation_set = val
      )
    })
  })

  # flatten nested structure
  Vfolds_rolling_window_pooled <- unlist(Vfolds_rolling_window_pooled,
    recursive = FALSE
  )
  return(Vfolds_rolling_window_pooled)
}
