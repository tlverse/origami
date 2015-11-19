library(origami)
library(SuperLearner)
N <- 200
p <- 6
X <- matrix(rnorm(N * p), N, p)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(0.2 * X[, 1] + 0.1 * X[, 2] - 0.2 * X[, 3] + 0.1 * X[, 3] * 
    X[, 4] - 0.2 * abs(X[, 4])))

SL.library <- c("SL.glmnet", "SL.glm", "SL.knn_stored", "SL.gam", "SL.mean")

folds <- make_folds(X)

# Original SuperLearner convert folds into the form SuperLearner will accept
vfolds <- lapply(folds, function(fold) {
    validation(fold = fold)
})

set.seed(1)  #cv in glmnet is randomized so we have to set a seed to consistent results
sl_result <- SuperLearner(Y = Y, X = X, SL.library = SL.library, method = method.NNLS(), 
    family = binomial(), cvControl = list(validRows = vfolds))



# Origami SuperLearner
set.seed(1)  #cv in glmnet is randomized so we have to set a seed to consistent results
osl_result <- origami_SuperLearner(Y = Y, X = X, SL.library = SL.library, method = method.NNLS(), 
    family = binomial(), folds = folds)


# compare results
equal_values <- function(e1, e2) {
    all.equal(e1, e2, check.attributes = FALSE, use.names = FALSE)
}
equal_values(sl_result$cvRisk, osl_result$cvRisk)
equal_values(sl_result$coef, osl_result$coef)
equal_values(sl_result$SL.predict, predict(osl_result$fullFit, newdata = X)$pred)


####################### CV.SuperLearner
set.seed(1)
cv_sl_result <- CV.SuperLearner(Y = Y, X = X, SL.library = SL.library, method = "method.NNLS", 
    family = binomial())

# NB: not yet feature complete with CV.SuperLearner
set.seed(1)
ocv_sl_result <- origami_CV.SuperLearner(Y = Y, X = X, SL.library = SL.library, method = method.NNLS(), 
    family = binomial())

summary(cv_sl_result)
print(ocv_sl_result)

preds <- ocv_sl_result$SL.predict 
