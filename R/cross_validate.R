#' @title Main cross-validation function
#' @description Applies \code{cvfun} to the folds using \code{foreach} and combines the results across folds using \code{combine_results}.
#' 
#' @param cv_fun a function that takes a 'fold' as it's first argument.
#' and returns a list of results from that fold.
#' @param folds a list of folds to loop over generated using \code{\link{make_folds}}.
#' @param ... other arguments passed to \code{cvfun}.
#' @param .parallel logical; should \code{\%dopar\%} be used instead of \code{\%do\%}, to evalute on folds in parallel. See \code{\link[foreach]{foreach}} for details.
#' @param .foreach_control list; arguments to \code{\link[foreach]{foreach}}.
#' @param .combine logical; should \code{\link{combine_results}} be called.
#' @param .combine_control list; arguments to \code{\link{combine_results}}.
#'
#' @return A list of results, combined across folds.
#' @export
#'
#' @example /inst/examples/cv_example.R
#'  
cross_validate <- function(cv_fun, folds, ..., .parallel = F, .foreach_control = list(), .combine = T, .combine_control = list()) {
    
    # determine if we should parallelize
    `%do_op%` <- `%do%`
    if (.parallel && getDoParRegistered()) 
        `%do_op%` <- `%dopar%`
    
    # so as to not stress out CRAN about this variable being missing, 
    # when it's defined by for each
    fold <- NULL 
    
    # main loop
    results <- do.call(foreach, c(list(fold = folds), .foreach_control)) %do_op% {
        cv_fun(fold, ...)
    }

    #remove error results
    error_idx <- which(sapply(results,function(x)"error"%in%class(x)))
    error_results <- list(index=error_idx,error=results[error_idx])
    results <- results[-1*error_idx]
    
    # verify that the folds returned similar results
    if (length(unique(lapply(results, length))) > 1) 
        stop("lists returned from folds are not the same length")
    if (length(unique(lapply(results, names))) > 1) 
        stop("names returned from folds are not consistent")
    
    
    # invert results - go from a list containing one list per fold to a list containing one list per result returned
    # by cv_fun
    results <- apply(do.call(rbind, results), 2, as.list)
    
    # combine results
    if (.combine) {
        results <- do.call(combine_results, c(list(results = results), .combine_control))
    }
    
    results$error_results=error_results
    
    return(results)
} 
