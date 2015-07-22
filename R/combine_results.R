#' @title guess_combiner
#' @description Maps data types into standard combiners that should be sensible.
#' @param result a single result.
#' @return A function to combine a list of such results.
guess_combiner <- function(result) {
    switch(class(result), data.frame = combiner_rbind, matrix = combiner_rbind, numeric = combiner_c, 
        character = combiner_c, integer = combiner_c, logical = combiner_c, factor = combiner_factor, 
        array = combiner_array, identity)
}


#' @title Combine results from different folds
#' @description Applies \link{combiners} - functions that collapse across a list of similarly structured results, to a list of such lists. 
#' 
#' @param results list; a list of lists, corresponding to each result, with the inner lists corresponding to results from each fold.
#' @param combiners list; a list with the same names results, containing combiner function names or functions for each result.
#' @param smart_combiners logical; if combiners are missing, should they be guessed from the data type of the results.
#' @details In theory you should never call this function directly, because it is called automatically by cross_validate. The defaults (combiners guessed based on data type), should work in most cases.
#' @return A list of combined results.
#' @seealso \link{combiners}
#' @export
combine_results <- function(results, combiners = NULL, smart_combiners = T) {
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

#' @title Combiners
#' @description Combiners are functions that collapse across a list of similarly structured results. These are standard idioms for combining lists of certain data types. 
#' @param x list; a list of similar results to be combined.
#' @return A combined results object.
#' @rdname combiners
#' @name combiners
NULL

#' @rdname combiners
#' @export
combiner_rbind <- function(x) {
    do.call(rbind, x)
}
#' @rdname combiners
#' @export
combiner_c <- function(x) {
    do.call(c, x)
}

#' @rdname combiners
#' @export
combiner_factor <- function(x) {
    unlist(x)
}

#' @rdname combiners
#' @export
combiner_array <- function(x) {
    do.call(abind, c(x, along = 1))
} 
