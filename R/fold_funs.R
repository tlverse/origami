#' @family fold_funs
#' @title Make list of folds for cross-validation
#' @description Generates a list of folds for a variety of cross-validation schemes.
#' 
#' @param n either an integer indicating the number of observations to cross-validate over, or an object from which to guess the number of observations. Can also be computed from strata_ids or cluster_ids.
#' @param fold_fun either a character vector or a function indicating the cross-validation scheme to use.
#' @param cluster_ids a vector of cluster ids. Clusters are treated as a unit - they are all placed in either the training or validation set.
#' @param strata_ids a vector of strata ids. Strata are balanced - insofar as possible the distribution in the sample should be the same as the distribution in the training and validation sets.
#' @param ... other arguments to \code{fold_fun}.
#' 
#' @return A list of Folds.
#' 
#' @seealso \code{link{Fold}}
#' 
#' @export
make_folds <- function(n = NULL, fold_fun = NULL, cluster_ids = NULL, strata_ids = NULL, ...) {
    if (missing(n)) {
        # compute n from strata or cluster ids if possible
        if (!is.null(strata_ids)) {
            n <- length(strata_ids)
        } else if (!is.null(cluster_ids)) {
            n <- length(cluster_ids)
        } else {
            stop("n not specified and there are no strata or cluster ids")
        }
        
    } else if (length(n) > 1) {
        # if n not an integer, use the number of rows or length of n
        if (!is.null(nrow(n))) {
            n <- nrow(n)
        } else {
            n <- length(n)
        }
    }
    
    if (!is.null(strata_ids)) {
        stopifnot(length(strata_ids) == n)
        
        if (!is.null(cluster_ids)) {
            stopifnot(length(cluster_ids) == n)
            
            # it's not clear what to do if clusters are not nested in strata, so we require this for now
            nesting <- all(rowSums(table(cluster_ids, strata_ids) > 0) == 1)
            if (!nesting) {
                stop("cluster ids are not nested in strata ids. This is currently unsupported")
            }
            
        }
        
        # generate separate folds for each strata
        folds <- strata_folds(fold_fun, cluster_ids, strata_ids, ...)
        
    } else if (!is.null(cluster_ids)) {
        # generate folds on clusters instead of observations
        stopifnot(length(cluster_ids) == n)
        folds <- cluster_folds(fold_fun, cluster_ids, ...)
        
    } else {
        # we either don't have clusters or strata, or we're in the functions that are handling those
        
        if (!is.function(fold_fun)) {
            # determine method
            fun_names <- c("vfold", "loo", "montecarlo", "bootstrap", "resubstitution")
            fold_funs <- c(folds_vfold, folds_loo, folds_montecarlo, folds_bootstrap, folds_resubstitution)
            fold_fun <- match.arg(fold_fun, fun_names)
            fold_fun <- fold_funs[[match(fold_fun, fun_names)]]
        }
        
        # generate folds
        folds <- fold_fun(n, ...)
    }
    
    return(folds)
}

# for v-fold type cross-validation, take a fold vector and make fold object for fold v
fold_from_foldvec <- function(v, folds) {
    training_set <- which(folds != v)
    validation_set <- which(folds == v)
    
    make_fold(v, training_set, validation_set)
}

#' @family fold_funs
#' @title V-fold cross-validation
#' @description V-fold cross-validation.
#' 
#' @param n integer; number of observations.
#' @param V integer; number of folds.
#' 
#' @return A list of Folds.
#' 
#' @export
folds_vfold <- function(n, V = 10) {
    if (n <= V) {
        warning("n<=V so using leave-one-out CV")
        return(folds_loo(n))
    }
    
    folds <- rep(seq_len(V), length = n)
    
    # shuffle folds
    folds <- sample(folds)
    
    # generate fold vectors
    folds <- lapply(1:V, fold_from_foldvec, folds)
    
    return(folds)
}

# use full sample for both training and validation
folds_resubstitution <- function(n) {
    list(make_fold(1L, seq_len(n), seq_len(n)))
}

#' @family fold_funs
#' @title Leave-one-out cross-validation
#' @description Leave-one-out cross-validation.
#' 
#' @param n integer; number of observations.
#' 
#' @return A list of Folds
#' 
#' @export
folds_loo <- function(n) {
    
    # folds are trivial here
    folds <- seq_len(n)
    
    # generate fold vectors
    folds <- lapply(folds, fold_from_foldvec, folds)
    
    return(folds)
}

# make monte carlo type folds
fold_montecarlo <- function(v, n, ntrain, replace) {
    training_set <- sample(n, size = ntrain, replace = replace)
    validation_set <- setdiff(seq_len(n), training_set)
    
    make_fold(v, training_set, validation_set)
}

#' @family fold_funs
#' @title Monte Carlo cross-validation
#' @description Monte Carlo cross-validation.
#' 
#' @param n integer; number of observations.
#' @param V integer; number of folds to generate.
#' @param pvalidation proportion of observation to be in validation fold.
#' 
#' @export
folds_montecarlo <- function(n, V = 1000, pvalidation = 0.2) {
    stopifnot(pvalidation > 0 && pvalidation < 1)
    
    # calculate training sample size
    ntrain <- round((1 - pvalidation) * n)
    stopifnot(ntrain > 1)
    
    folds <- lapply(seq_len(V), fold_montecarlo, n, ntrain, replace = F)
    
    return(folds)
}

#' @family fold_funs
#' @title Bootstrap cross-validation
#' @description Bootstrap cross-validation.
#' 
#' @param n integer; number of observations
#' @param V integer; number of folds to generate.
#' 
#' @export
folds_bootstrap <- function(n, V = 1000) {
    folds <- lapply(seq_len(V), fold_montecarlo, n, n, replace = T)
    
    return(folds)
}

# generate folds for clusters, and then convert into folds for observations
cluster_folds <- function(fold_fun, cluster_ids, ...) {
    # convert ids to numeric 1:n
    idfac <- factor(cluster_ids)
    nclusters <- length(levels(idfac))
    clusternums <- as.numeric(idfac)
    
    # generate folds for ids
    nclusters <- length(unique(clusternums))
    idfolds <- make_folds(n = nclusters, fold_fun = fold_fun, cluster_ids = NULL, ...)
    
    # convert this into folds for observations
    folds <- lapply(idfolds, function(idfold) {
        make_fold(v = fold_index(fold = idfold), training_set = which(clusternums %in% training(clusternums, idfold)), 
            validation_set = which(clusternums %in% validation(clusternums, idfold)))
    })
    
    return(folds)
}

# generate folds separaetly for each strata, and then collapse
strata_folds <- function(fold_fun, cluster_ids, strata_ids, ...) {
    # convert strata to numeric 1:n
    idfac <- factor(strata_ids)
    nstrata <- length(levels(idfac))
    stratanums <- as.numeric(idfac)
    
    # generate strata specific folds
    strata_folds <- lapply(seq_len(nstrata), function(strata) {
        n_in_strata <- sum(stratanums == strata)
        idfolds <- make_folds(n = n_in_strata, fold_fun = fold_fun, cluster_ids = cluster_ids[stratanums == strata], 
            strata_ids = NULL, ...)
    })
    
    # collapse strata folds
    V <- length(strata_folds[[1]])
    
    folds <- lapply(seq_len(V), function(v) {
        # convert to indexes on the observations
        converted_folds <- lapply(seq_len(nstrata), function(strata) {
            strata_idx <- which(stratanums == strata)
            strata_fold <- strata_folds[[strata]][[v]]
            make_fold(v = v, training_set = training(strata_idx, strata_fold), validation_set = validation(strata_idx, 
                strata_fold))
            
        })
        
        # collapse across strata
        make_fold(v = v, training_set = unlist(lapply(converted_folds, function(fold) {
            training(fold = fold)
        })), validation_set = unlist(lapply(converted_folds, function(fold) {
            validation(fold = fold)
        })))
    })
    
    return(folds)
}


#' @family fold_funs
#' @title Rolling origin cross-validation
#' @description Rolling origin cross-validation.
#' 
#' @param n integer; number of observations.
#' @param first_window integer; number of observations in the first training sample.
#' @param validation_size integer; number of points in the validation samples. Should be equal to the largest forecast horizon
#' 
#' @export
folds_rolling_origin <- function(n, first_window, validation_size) {
  last_window <- n-validation_size
  origins <- first_window:last_window
  folds <- lapply(seq_along(origins), function(i){
    origin <- origins[i]
    make_fold(v=i, training_set=1:origin, validation_set=origin+(1:validation_size))
  })
  
  return(folds)
}

#' @family fold_funs
#' @title Rolling window cross-validation
#' @description Rolling window cross-validation.
#' 
#' @param n integer; number of observations.
#' @param window_size integer; number of observations in the training samples.
#' @param validation_size integer; number of points in the validation samples. Should be equal to the largest forecast horizon
#' 
#' @export
folds_rolling_window <- function(n, window_size, validation_size) {
  last_window <- n-validation_size
  origins <- window_size:last_window
  folds <- lapply(seq_along(origins), function(i){
    origin <- origins[i]
    make_fold(v=i, training_set=(1:window_size)+(i-1L), validation_set=origin+(1:validation_size))
  })
  
  return(folds)
}