.SL.require <- getFromNamespace(".SL.require", "SuperLearner")

# this doesn't work yet because glmnet can pick different sets of lambdas - need to fix this
SL.glmnetall <- function(Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10, nlambda = 100, useMin = TRUE, 
    ...) {
    .SL.require("glmnet")
    if (!is.matrix(X)) {
        X <- model.matrix(~-1 + ., X)
        newX <- model.matrix(~-1 + ., newX)
    }
    fit <- glmnet(x = X, y = Y, weights = obsWeights, lambda = NULL, family = family$family, alpha = alpha, nlambda = nlambda)
    pred <- predict(fit, newx = newX, type = "response")
    predout <- matrix(0, nrow = nrow(newX), ncol = nlambda)
    predout[, 1:ncol(pred)] <- pred
    fit <- list(object = fit)
    class(fit) <- "SL.glmnetall"
    out <- list(pred = predout, fit = fit)
    return(out)
}

predict.SL.glmnetall <- function(object, newdata, ...) {
    if (!is.matrix(newdata)) {
        newdata <- model.matrix(~-1 + ., newdata)
    }
    
    pred <- predict(object$object, newx = newdata, type = "response")
    return(pred)
} 
