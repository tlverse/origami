#' Flexible Guessing and Mapping for Combining Data Types
#'
#' Maps data types into standard combiners that should be sensible.
#'
#' @param result A single result; flexibly accepts several object classes.
#'
#' @importFrom data.table rbindlist
#'
#' @return A function to combine a list of such results.
guess_combiner <- function(result) {
  switch(class(result)[1],
    data.table = data.table::rbindlist,
    data.frame = combiner_rbind,
    matrix = combiner_rbind,
    numeric = combiner_c,
    character = combiner_c,
    integer = combiner_c,
    logical = combiner_c,
    factor = unlist,
    array = combiner_array,
    identity
  )
}

################################################################################

#' Combine Results from Different Folds
#'
#' Applies \code{\link{Combiners}}: functions that collapse across a list of
#' similarly structured results, to a list of such lists.
#'
#' @param results A \code{list} of \code{list}s, corresponding to each result,
#'  with the inner \code{list}s corresponding to results from each fold.
#' @param combiners A \code{list} with the same names as results, containing
#'  combiner function names or functions for each result.
#' @param smart_combiners A \code{logical} indicating whether combiners should
#'  be guessed from the data type of the results if they are missing.
#'
#' @details In theory you should never call this function directly, because it
#'  is called automatically by cross_validate. The defaults, combiners guessed
#'  based on data type, should work in most cases.
#'
#' @return A list of combined results.
#'
#' @seealso \code{\link{Combiners}}
#'
#' @export
combine_results <- function(results,
                            combiners = NULL,
                            smart_combiners = TRUE) {
  result_names <- names(results)

  if (is.null(combiners) && smart_combiners) {
    # try to learn from data structure
    first_fold <- lapply(results, function(result) result[[1]])
    combiners <- sapply(first_fold, guess_combiner)
  }

  # combine results from different folds using the appropriate combiner
  if (!is.null(combiners)) {
    combined <- lapply(result_names, function(result_name) {
      combiners[[result_name]](results[[result_name]])
    })
    names(combined) <- result_names
  } else {
    combined <- results
  }
  return(combined)
}

################################################################################

#' Combiners
#'
#' Combiners are functions that collapse across a list of similarly structured
#' results. These are standard idioms for combining lists of certain data types.
#'
#' @param x A \code{list} of similar results to be combined.
#'
#' @return A combined results object.
#'
#' @rdname Combiners
#'
#' @name Combiners
NULL

#' @rdname Combiners
#'
#' @export
combiner_rbind <- function(x) {
  do.call(rbind, x)
}

#' @rdname Combiners
#'
#' @export
combiner_c <- function(x) {
  do.call(c, x)
}

#' @rdname Combiners
#'
#' @export
combiner_factor <- function(x) {
  unlist(x)
}

#' @rdname Combiners
#'
#' @importFrom abind abind
#'
#' @export
combiner_array <- function(x) {
  do.call(abind::abind, c(x, along = 1))
}
