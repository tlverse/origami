#' Make List of Folds for cross-validation
#'
#' Generates a list of folds for a variety of cross-validation schemes.
#'
#' @family fold generation functions
#'
#' @param n - either an integer indicating the number of observations to
#'  cross-validate over, or an object from which to guess the number of
#'  observations; can also be computed from \code{strata_ids} or
#'  \code{cluster_ids}.
#' @param fold_fun - A function indicating the cross-validation scheme to use.
#'  See \code{\link{fold_funs}} for a list of possibilities.
#' @param cluster_ids - a vector of cluster ids. Clusters are treated as a unit
#'  -- that is, all observations within a cluster are placed in either the
#'  training or validation set.
#' @param strata_ids - a vector of strata ids. Strata are balanced: insofar as
#'  possible the distribution in the sample should be the same as the
#'  distribution in the training and validation sets.
#' @param ... other arguments to be passed to \code{fold_fun}.
#'
#' @return A list of folds objects. Each fold consists of a list with a
#'  \code{training} index vector, a \code{validation} index vector, and a
#'  \code{fold_index} (its order in the list of folds).
#'
#' @export
make_folds <- function(n = NULL,
                       fold_fun = folds_vfold,
                       cluster_ids = NULL,
                       strata_ids = NULL,
                       ...) {
  if (missing(n)) {
    # compute n from strata or cluster ids if possible
    if (!is.null(strata_ids)) {
      n <- length(strata_ids)
    } else if (!is.null(cluster_ids)) {
      n <- length(cluster_ids)
    } else {
      stop("n not specified and there are no strata or cluster IDs.")
    }
  } else if (length(n) > 1) {
    # if n not an integer, use the number of rows or length of n
    if (!is.null(nrow(n))) {
      n <- nrow(n)
    } else {
      n <- length(n)
    }
  }

  if ("t" %in% names(formals(fold_fun)) &
    (!is.null(cluster_ids) | !is.null(strata_ids))) {
    stop(
      "cluster_ids and strata_ids are not supported by fold functions ",
      "for time series cross-validation schemes. See the time series fold ",
      "function's specific arguments to check what is supported."
    )
  }

  if (!is.null(strata_ids)) {
    stopifnot(length(strata_ids) == n)

    if (!is.null(cluster_ids)) {
      stopifnot(length(cluster_ids) == n)

      # it's not clear what to do if clusters are not nested in
      # strata, so we require this for now
      nesting <- all(rowSums(table(cluster_ids, strata_ids) > 0) == 1)

      if (!nesting) {
        stop("cluster IDs are not nested in strata IDs. This is currently unsupported.")
      }
    }

    # generate separate folds for each strata
    folds <- strata_folds(fold_fun, cluster_ids, strata_ids, ...)
  } else if (!is.null(cluster_ids)) {
    # generate folds on clusters instead of observations
    stopifnot(length(cluster_ids) == n)
    folds <- cluster_folds(fold_fun, cluster_ids, ...)
  } else {
    # we either don't have clusters or strata, or we're in the
    # functions that are handling those
    # generate folds
    folds <- fold_fun(n, ...)
  }
  return(folds)
}

################################################################################

#' Convert ID Folds to Observation Folds
#'
#' This function convertsf olds that subset ids to folds that subset
#'  observations
#'
#' @param idfolds folds that subset ids
#' @param cluster_ids a vector of cluster ids indicating which observations are
#'  in which clusters
#'
#' @export
id_folds_to_folds <- function(idfolds, cluster_ids) {
  idfac <- factor(cluster_ids)
  nclusters <- length(levels(idfac))
  clusternums <- as.numeric(idfac)

  reindex <- function(index, fold_index) {
    which(index %in% fold_index)
  }

  folds <- lapply(idfolds, function(idfold) {
    make_fold(
      v = fold_index(fold = idfold),
      training_set = reindex(clusternums, training(fold = idfold)),
      validation_set = reindex(clusternums, validation(fold = idfold))
    )
  })

  return(folds)
}

# Generate folds for clusters, and then convert into folds for observations
cluster_folds <- function(fold_fun, cluster_ids, ...) {
  # convert ids to numeric 1:n
  idfac <- factor(cluster_ids)
  nclusters <- length(levels(idfac))
  clusternums <- as.numeric(idfac)
  # id_indexes <- by(seq_along(cluster_ids), list(id = clusternums), list)

  # generate folds for ids
  idfolds <- make_folds(
    n = nclusters, fold_fun = fold_fun,
    cluster_ids = NULL, ...
  )

  folds <- id_folds_to_folds(idfolds, cluster_ids)
  return(folds)
}

################################################################################

# generate folds separately for each strata, and then collapse
strata_folds <- function(fold_fun, cluster_ids, strata_ids, ...) {

  # order strata by increasing prevalence, so rarest strata is listed first
  ordered <- names(sort(prop.table(table(strata_ids)), decreasing = FALSE))
  idfac <- sort(factor(strata_ids, levels = ordered))
  nstrata <- length(levels(idfac))
  stratanums <- as.numeric(idfac)

  ##### generate strata-specific folds
  # create list args for calling make_folds
  args <- list(...)
  if (is.null(args$V) & "V" %in% names(formals(fold_fun))) {
    args$V <- formals(fold_fun)$V
  }
  args$fold_fun <- fold_fun
  args$cluster_ids <- cluster_ids
  # if number of observations in rarest strata < V, then V needs to change for
  # all strata-specific folds, so V is same across all strata-specific folds
  n_rare <- sum(stratanums == 1)
  if (!is.null(args$V) && n_rare < args$V) {
    warning(paste0(
      "The number of observations in the most rare strata is ", n_rare,
      ", and V is ", args$V, ", so using leave-one-out CV, i.e. setting V = n"
    ))
    args$V <- n_rare
  }
  strata_folds <- lapply(seq_len(nstrata), function(strata) {
    args_strata <- args
    args_strata$n <- sum(stratanums == strata)
    if (is.null(args$cluster_ids)) {
      args_strata <- args_strata[names(args_strata) %in% names(formals(fold_fun))]
      suppressWarnings(do.call(args$fold_fun, args_strata))
    } else {
      args_strata$cluster_ids <- args$cluster_ids[stratanums == strata]
      do.call(make_folds, args_strata)
    }
  })

  ###### collapse strata-specific folds
  V <- length(strata_folds[[1]])
  folds <- lapply(seq_len(V), function(v) {
    # convert to indexes on the observations
    converted_folds <- lapply(seq_len(nstrata), function(strata) {
      strata_idx <- which(stratanums == strata)
      strata_fold <- strata_folds[[strata]][[v]]
      make_fold(
        v = v, training_set = training(strata_idx, strata_fold),
        validation_set = validation(strata_idx, strata_fold)
      )
    })

    # collapse across strata
    make_fold(
      v = v,
      training_set = unlist(
        lapply(converted_folds, function(fold) training(fold = fold))
      ),
      validation_set = unlist(
        lapply(converted_folds, function(fold) validation(fold = fold))
      )
    )
  })

  return(folds)
}

################################################################################

#' Repeated Cross-Validation
#'
#' Implementation of repeated window cross-validation: generates fold objects
#' for repeated cross-validation by making repeated calls to
#' \code{\link{make_folds}} and concatenating the results.
#'
#' @family fold generation functions
#'
#' @param repeats An integer indicating the number of repeats.
#' @param ... Arguments passed to \code{\link{make_folds}}.
#'
#' @export
make_repeated_folds <- function(repeats, ...) {
  all_folds <- lapply(seq_len(repeats), function(x) make_folds(...))
  folds <- unlist(all_folds, recursive = FALSE)
  return(folds)
}
