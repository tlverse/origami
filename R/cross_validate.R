#' Main Cross-Validation Function
#'
#' Applies \code{cv_fun} to the folds using \code{future_lapply} and combines
#' the results across folds using \code{\link{combine_results}}.
#'
#' @param cv_fun A function that takes a 'fold' as it's first argument and
#'  returns a list of results from that fold. NOTE: the use of an argument
#'  named 'X' is specifically disallowed in any input function for compliance
#'  with the functions \code{\link[future.apply]{future_lapply}} and
#'  \code{lapply}.
#' @param folds A list of folds to loop over generated using
#'  \code{\link{make_folds}}.
#' @param ... Other arguments passed to \code{cvfun}.
#' @param use_future A \code{logical} option for whether to run the main loop
#'  of cross-validation with \code{\link[future.apply]{future_lapply}} or with
#'  \code{lapply}.
#' @param .combine A \code{logical} indicating if \code{\link{combine_results}}
#'  should be called.
#' @param .combine_control A \code{list} of arguments to
#'  \code{\link{combine_results}}.
#' @param .old_results A \code{list} containing the returned result from a
#'  previous call to this function. Will be combined with the current results.
#'  This is useful for adding additional CV folds to a results object.
#'
#' @importFrom assertthat assert_that
#' @importFrom future future plan sequential multicore multisession
#' @importFrom future.apply future_lapply
#' @importFrom listenv listenv
#' @import methods
#'
#' @return A \code{list} of results, combined across folds.
#'
#' @export
#'
#' @example /inst/examples/cv_simple_example.R
#' @example /inst/examples/cv_parallel_example.R
cross_validate <- function(cv_fun,
                           folds,
                           ...,
                           use_future = TRUE,
                           .combine = TRUE,
                           .combine_control = list(),
                           .old_results = NULL) {

  # argument 'X' is specifically disallowed since reserved by lapply family
  args_cv_fun <- names(formals(cv_fun))
  assertthat::assert_that(!("X" %in% args_cv_fun),
    msg = "Argument name 'X' not allowed in cv_fun"
  )

  # catch and return errors if they occur
  wrapped_fun <- wrap_in_try(cv_fun)

  # main loop
  if (use_future) {
    results <- future.apply::future_lapply(folds, wrapped_fun, ...,
      future.seed = TRUE
    )
  } else {
    results <- lapply(folds, wrapped_fun, ...)
  }

  # remove error results
  error_idx <- which(sapply(results, inherits, "try-error"))
  error_results <- list(index = error_idx, error = results[error_idx])
  good_results <- setdiff(seq_along(folds), error_idx)
  results <- results[good_results]

  if (length(results) > 0) {
    # verify that the folds returned similar results
    if (length(unique(lapply(results, length))) > 1) {
      stop("lists returned from folds are not the same length")
    }
    if (length(unique(lapply(results, names))) > 1) {
      stop("names returned from folds are not consistent")
    }

    # invert results - go from a list containing one list per fold to a list
    # containing one list per result returned by cv_fun
    results <- apply(do.call(rbind, results), 2, as.list)

    # combine results
    if (.combine) {
      results <- do.call(
        combine_results,
        c(
          list(results = results),
          .combine_control
        )
      )
    }
  } else {
    warning("All iterations resulted in errors")
  }

  results$errors <- error_results

  if (!is.null(.old_results)) {
    # invert results - go from a list containing one list per fold to a list
    # containing one list per result returned by cv_fun
    new_and_old <- list(results, .old_results)
    new_and_old <- apply(do.call(rbind, new_and_old), 2, as.list)
    results <- combine_results(results = new_and_old)
  }
  return(results)
}
