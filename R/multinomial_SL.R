# Super Learner for Multinomial A (also multinomial probabilities?)
mn_loglik <- function(pred, truth, weight) {
    return(weighted.mean(rowSums(truth * log(pred)), weight))
}

trim_loglin <- function(x, trim = 1e-05) {
    x[x < trim] <- trim
    x[x > (1 - trim)] <- (1 - trim)
    
    return(log(x))
}

#' @export
mn_pred <- function(alpha, x) {
    Y_pred <- plogis(t(aaply(x, 2, `%*%`, alpha)))
    
    # normalize so class predictions sum to 1
    Y_pred <- normalize_rows(Y_pred)
    
    return(Y_pred)
}

#' @export
method.mnNNloglik <- function() {
    out <- list(require = NULL, computeCoef = function(Z, Y, libraryNames, verbose, 
        obsWeights, control, ...) {
        Y_ind <- factor_to_indicators(Y)
        cvRisk <- -2 * aaply(Z, 3, mn_loglik, Y_ind, obsWeights)
        
        names(cvRisk) <- libraryNames
        .NNloglik <- function(x, truth, weight, start_alpha = rep(0, dim(x)[3])) {
            fmin <- function(alpha, x, truth, weight) {
                Y_pred <- mn_pred(alpha, x)
                result <- -2 * nrow(Y_pred) * mn_loglik(Y_pred, truth, weight)
                if (!is.finite(result)) browser()
                return(result)
            }
            gmin <- function(alpha, x, truth, weight) {
                eta <- t(aaply(x, 2, `%*%`, alpha))
                p <- plogis(eta)
                d <- dlogis(eta)
                psum <- rowSums(p)
                # dsum=rowSums(d)
                a_fac <- weight * truth * (d/p)
                a_grad <- aaply(x, 3, function(x_a) (sum(a_fac * x_a)))
                tot_fac <- d/psum
                tot_grad <- aaply(x, 3, function(x_a) (sum(tot_fac * x_a)))
                -2 * (a_grad - tot_grad)
            }
            fit <- optim(start_alpha, fmin, gmin, x = x, truth = truth, weight = weight, 
                method = "L-BFGS-B", lower = 0, upper = 1)  #, ...)
            fit
            invisible(fit)
        }
        tempZ <- trimLogit(Z)
        fit.nnloglik <- .NNloglik(x = tempZ, truth = Y_ind, weight = obsWeights)
        if (verbose) {
            message(paste("Non-Negative log-likelihood convergence: ", fit.nnloglik$convergence == 
                0))
        }
        initCoef <- fit.nnloglik$par
        initCoef[initCoef < 0] <- 0
        initCoef[is.na(initCoef)] <- 0
        if (sum(initCoef) > 0) {
            coef <- initCoef/sum(initCoef)
        } else {
            warning("All algorithms have zero weight", call. = FALSE)
            coef <- initCoef
        }
        out <- list(cvRisk = cvRisk, coef = coef)
        return(out)
    }, computePred = function(predY, coef, control, ...) {
        out <- mn_pred(coef, trim_loglin(predY))
        return(out)
    })
    invisible(out)
}

#' @title multinomial_SuperLearner
#' @description Convenience function to specify mulitnomial wrappers, family, and combination method. 
#'        See \code{\link{predict.origami_SuperLearner}} for details of other arguments 
#' @seealso \code{\link{predict.origami_SuperLearner}}
#' @export

multinomial_SuperLearner <- function(Y, X, SL.library = c("mnSL.randomForest", "mnSL.glmnet", 
    "mnSL.multinom", "mnSL.mean"), ...) {
    origami_SuperLearner(Y = factor(Y), X = X, SL.library = SL.library, family = list(family = "multinomial"), 
        method = method.mnNNloglik(), ...)
} 
