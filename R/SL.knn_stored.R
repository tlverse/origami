# Modified version of SL.knn from the SuperLearner package by Eric Polley <eric.polley at nih.gov> Stores X and Y in the fit
# object so they don't have to be provided for new predictions knn{class} will only work with binomial to create additional
# algorithms with different values of k, for example k=20 SL.knn20 <- function(..., k = 20) SL.knn(...,k = k)
.SL.require <- getFromNamespace(".SL.require", "SuperLearner")

#' @title New learners
#' @name new_learners
#' @description Learners that build on those in the SuperLearner package.
#' @rdname new_learners
#' @aliases SL.knn_stored predict.SL.knn_stored
#' @export SL.knn_stored predict.SL.knn_stored
NULL


SL.knn_stored <- function(Y, X, newX, family, k = 10, ...) {
    
    .SL.require("class")
    if (family$family == "gaussian") {
        stop("SL.knn only available for family = binomial()")
    }
    fit.knn <- knn(train = X, test = newX, k = k, cl = Y, prob = TRUE)
    pred <- (as.numeric(fit.knn) - 1) * attr(fit.knn, "prob") + (1 - (as.numeric(fit.knn) - 1)) * (1 - attr(fit.knn, "prob"))
    fit <- list(k = k, X = X, Y = Y)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.knn_stored")
    return(out)
}

predict.SL.knn_stored <- function(object, newdata, ...) {
    .SL.require("class")
    fit.knn <- knn(train = object$X, test = newdata, k = object$k, cl = object$Y, prob = TRUE)
    pred <- (as.numeric(fit.knn) - 1) * attr(fit.knn, "prob") + (1 - (as.numeric(fit.knn) - 1)) * (1 - attr(fit.knn, "prob"))
    return(pred)
}
