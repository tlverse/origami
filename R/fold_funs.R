#' Cross-Validation Schemes
#'
#' These functions represent different cross-validation schemes that can be
#' used with \pkg{origami}. They should be used as options for the
#' \code{fold_fun} argument to \code{\link{make_folds}}, which will call the
#' requested function specify \code{n}, based on its arguments, and pass any
#' remaining arguments (e.g. \code{V} or \code{pvalidation}) on.
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
#' @param t An integer indicating the total amount of time to consider per
#'  time-series sample.
#' @param time An optional vector of integers of time points observed for each
#'  subject in the sample.
#' @param id An optional vector of unique identifiers corresponding to the time
#'  vector. These can be used to subset the time vector.
#'  @param subgroup Binary indicator vector of membership in the subgroup of interest for subgroup-specific cross-validation (eg data$S).
#'  @param strat_outcome Optional binary outcome vector (eg data$Y) for subgroup-specific cross-validation if equal proportion of outcome in training and validation folds desired.
#'
#' @importFrom assertthat assert_that
#'
#' @return A list of \code{Fold}s.
#' @name fold_funs
NULL

###############################################################################

#' @rdname fold_funs
#' @export
folds_vfold <- function(n, V = 10L) {
  if (n <= V) {
    warning("n <= V so using leave-one-out CV")
    return(folds_loo(n))
  }
  folds <- rep(seq_len(V), length = n)

  # shuffle folds
  folds <- sample(folds)

  # generate fold vectors
  folds <- lapply(seq_len(V), fold_from_foldvec, folds)
  return(folds)
}

###############################################################################

#' @rdname fold_funs
#' @export
folds_resubstitution <- function(n) {
  list(make_fold(1L, seq_len(n), seq_len(n)))
}

###############################################################################

#' @rdname fold_funs
#' @export
folds_loo <- function(n) {
  # folds are trivial here
  folds <- seq_len(n)

  # generate fold vectors
  folds <- lapply(folds, fold_from_foldvec, folds)
  return(folds)
}

###############################################################################

#' @rdname fold_funs
#' @export
folds_montecarlo <- function(n, V = 1000L, pvalidation = 0.2) {
  assertthat::assert_that(pvalidation > 0 && pvalidation < 1)

  # calculate training sample size
  ntrain <- round((1 - pvalidation) * n)
  assertthat::assert_that(ntrain > 1)

  folds <- lapply(seq_len(V), fold_montecarlo, n, ntrain, replace = FALSE)
  return(folds)
}

# make monte carlo type folds
fold_montecarlo <- function(v, n, ntrain, replace) {
  training_set <- sample(n, size = ntrain, replace = replace)
  validation_set <- setdiff(seq_len(n), training_set)
  make_fold(v, training_set, validation_set)
}

###############################################################################

#' @rdname fold_funs
#' @export
folds_bootstrap <- function(n, V = 1000L) {
  folds <- lapply(seq_len(V), fold_montecarlo, n, n, replace = TRUE)
  return(folds)
}

###############################################################################

#' @rdname fold_funs
#' @export
folds_rolling_origin <- function(n, first_window, validation_size, gap = 0L,
                                 batch = 1L) {
  last_window <- n - (validation_size + gap)
  origins <- seq.int(first_window, last_window, by = batch)

  folds <- lapply(seq_along(origins), function(i) {
    origin <- origins[i]
    make_fold(
      v = i, training_set = seq_len(origin),
      validation_set = origin + gap + (seq_len(validation_size))
    )
  })
  return(folds)
}

###############################################################################

#' @rdname fold_funs
#' @export
folds_rolling_window <- function(n, window_size, validation_size, gap = 0L,
                                 batch = 1L) {
  last_window <- n - (validation_size + gap)
  origins <- seq.int(window_size, last_window, by = batch)

  folds <- lapply(seq_along(origins), function(i) {
    origin <- origins[i]
    make_fold(
      v = i, training_set = (seq_len(window_size)) + (i * batch - batch),
      validation_set = origin + gap + (seq_len(validation_size))
    )
  })
  return(folds)
}

###############################################################################

#' @rdname fold_funs
#' @export
folds_rolling_origin_pooled <- function(n, t, id = NULL, time = NULL,
                                        first_window, validation_size,
                                        gap = 0L, batch = 1L) {
  # check compatibility of ID and time arguments
  check_id_and_time(id = id, time = time)

  # make IDs the same length as time if only one ID provided
  if (length(id) == 1L) {
    id <- rep(id, length(time))
  }

  # make skeleton dataset with IDs and times
  if (is.null(id) & is.null(time)) {
    dat <- cbind.data.frame(
      time = rep(seq(t), n / t),
      id = rep(seq(n / t), each = t)
    )
  } else {
    # Index times by id (allows variability in time observed for each subject)
    dat <- cbind.data.frame(time = time, id = id)
  }

  ids <- unique(dat$id)
  times <- unique(dat$time)
  message(paste("Processing", length(ids), "samples with", t, "time points."))

  # establish rolling origin forecast for time-series cross-validation
  rolling_origin_skeleton <- folds_rolling_origin(
    t, first_window, validation_size, gap, batch
  )

  folds_rolling_origin <- lapply(rolling_origin_skeleton, function(fold) {
    train_times <- training(times)
    valid_times <- validation(times)
    train_idx <- which(dat$time %in% train_times)
    valid_idx <- which(dat$time %in% valid_times)
    fold <- make_fold(fold_index(), train_idx, valid_idx)
  })
  return(folds_rolling_origin)
}

###############################################################################

#' @rdname fold_funs
#' @export
folds_rolling_window_pooled <- function(n, t, id = NULL, time = NULL,
                                        window_size, validation_size,
                                        gap = 0L, batch = 1L) {
  # check compatibility of ID and time arguments
  check_id_and_time(id = id, time = time)

  # make IDs the same length as time if only one ID provided
  if (length(id) == 1L) {
    id <- rep(id, length(time))
  }

  # make skeleton dataset with IDs and times
  if (is.null(id) & is.null(time)) {
    dat <- cbind.data.frame(
      time = rep(seq(t), n / t),
      id = rep(seq(n / t), each = t)
    )
  } else {
    # Index times by id (allows variability in time observed for each subject)
    dat <- cbind.data.frame(time = time, id = id)
  }

  ids <- unique(dat$id)
  times <- unique(dat$time)
  message(paste("Processing", length(ids), "samples with", t, "time points."))

  # establish rolling window forecast for time-series cross-validation
  rolling_window_skeleton <- folds_rolling_window(
    t, window_size,
    validation_size, gap, batch
  )

  folds_rolling_window <- lapply(rolling_window_skeleton, function(fold) {
    train_times <- training(times)
    valid_times <- validation(times)
    train_idx <- which(dat$time %in% train_times)
    valid_idx <- which(dat$time %in% valid_times)
    fold <- make_fold(fold_index(), train_idx, valid_idx)
  })
  return(folds_rolling_window)
}

###############################################################################

#' @rdname fold_funs
#' @export
folds_vfold_rolling_origin_pooled <- function(n, t, id = NULL, time = NULL,
                                              V = 10L, first_window,
                                              validation_size, gap = 0L,
                                              batch = 1L) {
  # check compatibility of ID and time arguments
  check_id_and_time(id = id, time = time)

  # make IDs the same length as time if only one ID provided
  if (length(id) == 1L) {
    id <- rep(id, length(time))
  }

  # make skeleton dataset with IDs and times
  if (is.null(id) & is.null(time)) {
    dat <- cbind.data.frame(
      time = rep(seq(t), n / t),
      id = rep(seq(n / t), each = t)
    )
  } else {
    # Index times by id (allows variability in time observed for each subject)
    dat <- cbind.data.frame(time = time, id = id)
  }

  ids <- unique(dat$id)
  times <- unique(dat$time)
  message(paste("Processing", length(ids), "samples with", t, "time points."))

  # establish V folds for cross-validating ids
  Vfold_allocation <- sample(rep(seq_len(V), length = length(ids)))
  Vfolds_skeleton <- lapply(seq_len(V), fold_from_foldvec, Vfold_allocation)

  # establish rolling origin forecast for time-series cross-validation
  rolling_origin_skeleton <- folds_rolling_origin(
    t, first_window,
    validation_size, gap, batch
  )

  # Put it all together: gives V-fold and rolling structure
  Vfolds_rolling_origin_pooled <- lapply(Vfolds_skeleton, function(vfold) {

    # Fold-specific Sample Index
    vfold_train_id <- vfold$training_set
    vfold_valid_id <- vfold$validation_set
    vfold_train_idx <- which(dat$id %in% vfold_train_id)
    vfold_valid_idx <- which(dat$id %in% vfold_valid_id)

    # Time
    folds_rolling_origin <- lapply(rolling_origin_skeleton, function(tfold) {
      train_times <- training(times, fold = tfold)
      valid_times <- validation(times, fold = tfold)
      train_idx <- which(dat$time %in% train_times)
      valid_idx <- which(dat$time %in% valid_times)
      fold_train_idx <- which(train_idx %in% vfold_train_idx)
      fold_valid_idx <- which(valid_idx %in% vfold_valid_idx)
      fold_idx <- length(rolling_origin_skeleton) * (vfold$v - 1L) + tfold$v
      make_fold(fold_idx, fold_train_idx, fold_valid_idx)
    })
  })

  folds <- unlist(Vfolds_rolling_origin_pooled, recursive = FALSE)
  return(folds)
}

###############################################################################

#' @rdname fold_funs
#' @export
folds_vfold_rolling_window_pooled <- function(n, t, id = NULL, time = NULL,
                                              V = 10L, window_size,
                                              validation_size, gap = 0L,
                                              batch = 1L) {
  # check compatibility of ID and time arguments
  check_id_and_time(id = id, time = time)

  # make IDs the same length as time if only one ID provided
  if (length(id) == 1) {
    id <- rep(id, length(time))
  }

  # make skeleton dataset with IDs and times
  if (is.null(id) & is.null(time)) {
    dat <- cbind.data.frame(
      time = rep(seq(t), n / t),
      id = rep(seq(n / t), each = t)
    )
  } else {
    # Index times by id (allows variability in time observed for each subject)
    dat <- cbind.data.frame(time = time, id = id)
  }

  ids <- unique(dat$id)
  times <- unique(dat$time)
  message(paste("Processing", length(ids), "samples with", t, "time points."))

  # establish V folds for cross-validating ids
  Vfold_allocation <- sample(rep(seq_len(V), length = length(ids)))
  Vfolds_skeleton <- lapply(seq_len(V), fold_from_foldvec, Vfold_allocation)

  # establish rolling origin forecast for time-series cross-validation
  rolling_window_skeleton <- folds_rolling_window(
    t, window_size,
    validation_size, gap, batch
  )

  # Put it all together: gives V-fold and rolling structure
  Vfolds_rolling_window_pooled <- lapply(Vfolds_skeleton, function(vfold) {
    # Fold-specific Sample Index
    vfold_train_id <- vfold$training_set
    vfold_valid_id <- vfold$validation_set
    vfold_train_idx <- which(dat$id %in% vfold_train_id)
    vfold_valid_idx <- which(dat$id %in% vfold_valid_id)

    # Time
    folds_rolling_origin <- lapply(rolling_window_skeleton, function(tfold) {
      train_times <- training(times, fold = tfold)
      valid_times <- validation(times, fold = tfold)
      train_idx <- which(dat$time %in% train_times)
      valid_idx <- which(dat$time %in% valid_times)
      fold_train_idx <- which(train_idx %in% vfold_train_idx)
      fold_valid_idx <- which(valid_idx %in% vfold_valid_idx)
      fold_idx <- length(rolling_window_skeleton) * (vfold$v - 1L) + tfold$v
      make_fold(fold_idx, fold_train_idx, fold_valid_idx)
    })
  })

  folds <- unlist(Vfolds_rolling_window_pooled, recursive = FALSE)
  return(folds)
}

#########################################################
#Add option to make subgroup-specific cross-validation folds

#' @rdname fold_funs
#' @export
folds_subgroup <- function(n, V = 10L, subgroup, strat_outcome = NULL) {
  
  if(is.null(strat_outcome)==TRUE){
    
    subgroup_index <- which(subgroup == 1)
    
    folds <- rep(seq_len(V), length = length(subgroup_index))
    
    # shuffle folds
    folds <- sample(folds)
    foldsdf <- data.frame(subgroup_index, folds)
    
    # generate fold vectors
    folds <- lapply(seq_len(V), fold_from_foldvec_subgroup, foldsdf, n)
    return(folds)
    
  }
  else {
    subgroup_index1 <- which(subgroup == 1 & strat_outcome == 1)
    subgroup_index2 <- which(subgroup == 1 & strat_outcome == 0)
    
    #Two sets of folds, one for outcome == 1 and one for outcome == 0
    folds1 <- rep(seq_len(V), length = length(subgroup_index1))
    folds2 <- rep(seq_len(V), length = length(subgroup_index2))
    
    # shuffle folds
    folds1 <- sample(folds1)
    foldsdf1 <- data.frame(subgroup_index1, folds1)
    
    folds2 <- sample(folds2)
    foldsdf2 <- data.frame(subgroup_index2, folds2)
    
    # generate fold vectors
    folds <- lapply(seq_len(V), fold_from_foldvec_subgroup_stratoutcome, foldsdf1, foldsdf2,  n)
    return(folds)
  }
}


###############################################################################

#' Check ID and Time Compatibility
#'
#' @param id An optional vector of unique identifiers corresponding to the time
#'  vector. These can be used to subset the time vector.
#' @param time An optional vector of integers of time points observed for each
#'  subject in the sample.
#'
#' @importFrom assertthat assert_that
#'
#' @keywords internal
check_id_and_time <- function(id, time) {
  # check that both time and ID are provided, or that neither are provided
  msg_time_id <- paste(
    "Cannot create flexible folds (allow for variability",
    "in the amount of time observed for each id) unless",
    "both `time` and `id` arguments are provided. Either",
    "provide both `time` and `id` or neither."
  )
  assertthat::assert_that(!(!is.null(id) & is.null(time)) ||
    !(is.null(id) & !is.null(time)),
  msg = msg_time_id
  )

  # check that observed times are provided for each ID
  msg_time_length <- paste(
    "Cannot create flexible folds (allow for",
    "variability in the amount of `time` observed for",
    "each `id`) unless `time` vector is of same length",
    "as `id` vector. `time` is a vector of integers of",
    "time points observed for each subject, and `id`",
    "is a vector of unique identifiers which",
    "correspond to the time vector. The `id` vector",
    "is used to subset the `time` vector."
  )
  if (length(id) > 1) {
    assertthat::assert_that(length(id) == length(time),
      msg = msg_time_length
    )
  }
}

