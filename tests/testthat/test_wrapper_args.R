library(origami)

context("Wrapper Args")

#wrapper that uses dot args for prediction
#modified from SL.mean
SL.arg_wrapper <- function (Y, X, newX, family, obsWeights, id, ...) 
{
  meanY <- weighted.mean(Y, w = obsWeights)
  pred <- rep.int(meanY, times = nrow(newX))
  fit <- list(object = meanY)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.arg_wrapper")
  return(out)
}

predict.SL.arg_wrapper <- function (object, newdata, family, X = NULL, Y = NULL, fake_mean=NULL, ...) 
{
   if(is.null(fake_mean)){
    meanY <- object$object
   } else {
     meanY <- fake_mean
   }
   pred <- rep.int(meanY, times = nrow(newdata))
  return(pred)
}

set.seed(1)
N <- 200
p <- 6
X <- matrix(rnorm(N * p), N, p)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(0.2 * X[, 1] + 0.1 * X[, 2] - 0.2 * X[, 3] + 0.1 * X[, 3] * X[, 4] - 0.2 * abs(X[, 4])))

assign("SL.arg_wrapper",SL.arg_wrapper,envir=globalenv())
assign("predict.SL.arg_wrapper",predict.SL.arg_wrapper,envir=globalenv())

SL.library <- "SL.arg_wrapper"

osl_result <- origami_SuperLearner(Y = Y, X = X, SL.library = SL.library, method = method.NNLS(), family = binomial())
meanY <- mean(Y)
mean_preds <- predict(osl_result,newdata=X)$pred
expected_preds <- matrix(meanY, nrow = nrow(X),ncol=1)

test_that("SL.arg_wrapper works without arguments passed",expect_equal(mean_preds,expected_preds))

fake_mean_preds <- predict(osl_result,newdata=X,fake_mean=0)$pred
expected_fake_preds <- matrix(0, nrow = nrow(X),ncol=1)

test_that("SL.arg_wrapper works with arguments passed",expect_equal(fake_mean_preds,expected_fake_preds))

