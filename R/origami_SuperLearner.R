# benefits over SuperLearner package from gencv: arbitrary CV schemes foreach
# parallelization from SL implementation, can return fold specific SL fits for
# smart sequential super learner -> don't fully match

# do this robust to errors
fitmods <- function(SL.library, Y, X, newX, family, obsWeights, id, ...) {
    fits <- lapply(SL.library, function(learner) {
        res <- NULL
        try({
            res <- do.call(learner, list(Y = Y, X = X, newX = newX, family = family, 
                obsWeights = obsWeights, id = id, ...))
        }, silent = FALSE)
        
        res
    })
    
    names(fits) <- SL.library
    return(fits)
}

#' @param fold a Fold to be passed to cv_SL.
#' @export
#' @rdname origami_SuperLearner
cv_SL <- function(fold, Y, X, SL.library, family, obsWeights, id, ...) {
    # training objects
    train_Y <- training(Y)
    train_X <- training(X)
    train_obsWeights <- training(obsWeights)
    train_id <- training(id)
    
    # validation objects
    valid_X <- validation(X)
    valid_index <- validation()
    
    # fit on training and predict on validation
    fits <- fitmods(SL.library, Y = train_Y, X = train_X, newX = valid_X, family = family, 
        obsWeights = train_obsWeights, id = train_id, ...)
    
    fit_failed <- sapply(fits, is.null)
    if (all(fit_failed)) {
        stop(sprintf("All learners failed for fold %d", fold_index()))
    } else if (any(fit_failed)) {
        dummy_pred <- fits[[which(!fit_failed)[1]]]$pred * 0
        for (failed_index in which(fit_failed)) {
            fits[[failed_index]] <- list(pred = dummy_pred, fit = NA)
        }
    }
    
    # extract and collapse predictions preds <- lapply(fits, function(fit) fit$pred)
    preds <- lapply(fits, function(fit) drop(fit$pred))
    Z <- do.call(abind, c(preds, rev.along = 0))
    
    results <- list(Z = Z, valid_index = valid_index, valY = validation(Y), valWeights = validation(obsWeights), 
        fits = fits, fit_failed = fit_failed)
    
    return(results)
}

#' @title origami_SuperLearner
#' @description SuperLearner implemented using orgami cross-validation. Leverages a lot of code from Eric Polley's 
#' SuperLearner package. Because of it's based on origami, we get two features for free: 
#' foreach based parallelization, and support for arbitrary cross-validation schemes. 
#' @param Y vector of outcomes.
#' @param X vector of covariates.
#' @param newX currently ignored.
#' @param SL.library character vector of learner names.
#' @param family Either gaussian() or binomial() depending on if Y is binary or continuous.
#' @param obsWeights vector of weights.
#' @param id vector of ids.
#' @param folds a list of Folds. See \code{\link{make_folds}}. If missing, 
#'        will be created based on Y and cluster_ids.
#' @param method a combination method. Typically either method.NNLS or method.NNloglik.
#' @param cvfun the function to be run at each cross-validation step. Changing this allows, 
#'        for example, dynamic creation of fold-specific data. Must have same prototype as cv_SL

#' @param ... other arguments passed to the underlying call to \code{\link{cross_validate}}
#' 
#' @seealso \code{\link{predict.origami_SuperLearner}}
#' @example /inst/examples/SL_example.R
#' 
#' @export
origami_SuperLearner <- function(Y, X, newX = NULL, SL.library, family = gaussian(), 
    obsWeights = rep(1, length(Y)), id = NULL, folds = NULL, method = method.NNLS(), 
    cvfun = cv_SL, control = list(), ...) {
    
    if (is.null(folds)) {
        folds <- make_folds(Y, cluster_ids = id)
    }
    
    if (is.null(newX)) {
        newX <- X
    }
    
    if (is.null(id)) {
        id <- seq_along(Y)
    }
    
    # fit algorithms to folds, get predictions
    results <- cross_validate(cvfun, folds, Y, X, SL.library, family, obsWeights, 
        id, ...)
    
    
    # unshuffle results
    Z <- aorder(results$Z, order(results$valid_index))
    valY <- aorder(results$valY, order(results$valid_index))
    # valY <- results$valY[order(results$valid_index)]
    valWeights <- results$valWeights[order(results$valid_index)]
    
    # identify which algorithms failed and drop them
    failed_ever <- unique(names(which(results$fit_failed)))
    if (length(failed_ever) > 0) {
        warning(sprintf("The following learners failed on at least one fold and will be dropped: %s", 
            paste(failed_ever, collapse = ", ")))
    }
    good_ind <- match(setdiff(SL.library, failed_ever), SL.library)
    
    # drop bad algorithms from Z
    last_dim <- length(safe_dim(Z))
    Z <- index_dim(Z, good_ind, last_dim)
    rownames(Z)=NULL
    SL.library <- SL.library[good_ind]
    
    # calculate coefficients
    SLcontrol <- SuperLearner.control(control)
    getCoef <- method$computeCoef(Z = Z, Y = valY, obsWeights = valWeights, libraryNames = SL.library, 
        verbose = F, control = SLcontrol)
    coef <- getCoef$coef
    cvRisk <- getCoef$cvRisk
    
    # refit models on full sample
    resub <- make_folds(Y, fold_fun = "resubstitution")[[1]]
    full <- cvfun(resub, Y, X, SL.library, family, obsWeights, id, ...)
    
    # fit object for predictions
    fitObj <- structure(list(library_fits = full$fits, coef = coef, family = family, 
        method = method, control = SLcontrol), class = "origami_SuperLearner_fit", 
        folds = folds)
    
    # analogous objects but with learners fit only in a particular fold
    foldFits <- lapply(seq_along(folds), function(fold) {
        fitObj$library_fits <- results$fits[[fold]][good_ind]
        fitObj
    })
    
    # results
    out <- list(coef = coef, cvRisk = cvRisk, Z = Z, valY = valY, valWeights = valWeights, 
        SL.library = SL.library, folds = folds, fullFit = fitObj, foldFits = foldFits)
    class(out) <- c("origami_SuperLearner", "SuperLearner")
    return(out)
}

#' @title predict.origami_SuperLearner
#' @description prediction function for origami_SuperLearner. Note, while this is a working SuperLearner implementation, it is intended more as an example 
#' than production code. As such, it is subject to change in the future.
#' @param object origami_SuperLearner fit.
#' @param newdata matrix or data.frame of new covariate values. If newdata='cv-original' (default), it will return Z (the split-specific library predictions), and the Super Learner applied to Z (the split-specific Super Learner Predictions)
#' @param ... other arguments to the learners.
#' @return A list with two elements: \code{library_pred} are the predictions from the library functions, and \code{pred} is the prediction from the SuperLearner.
#' @seealso \code{\link{origami_SuperLearner}}
#' 
#' @export
predict.origami_SuperLearner <- function(object, newdata = "cv-original", ...) {
    if (missing(newdata)) 
        (stop("newdata must be specified"))
    if (identical(newdata, "cv-original")) {
        Z <- object$Z
        pred_obj <- list(pred = object$fullFit$method$computePred(Z, object$fullFit$coef, 
            control = object$fullFit$control), library_pred = Z)
    } else {
        pred_obj <- predict(object$fullFit, newdata)
    }
    
    return(pred_obj)
}

#' @export
predict.origami_SuperLearner_fit <- function(object, newdata, ...) {
    if (missing(newdata)) 
        (stop("newdata must be specified"))
    
    library_pred <- lapply(seq_along(object$library_fits), function(index) {
        
        fitobj <- object$library_fits[[index]]
        pred <- predict(fitobj$fit, newdata = newdata, family = object$family)
        
        drop(pred)
    })
    
    Z <- do.call(abind, c(library_pred, rev.along = 0))
    
    pred <- object$method$computePred(Z, object$coef, control = object$control)
    
    return(list(pred = pred, library_pred = Z))
} 
