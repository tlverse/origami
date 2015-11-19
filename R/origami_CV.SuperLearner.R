#' @export
cv_cvsl <- function(fold, Y, X, id, obsWeights, ...) {
    # training objects
    train_Y <- training(Y)
    train_X <- training(X)
    train_obsWeights <- training(obsWeights)
    train_id <- training(id)
    
    # fit SuperLearner
    o_SL <- origami_SuperLearner(Y = train_Y, X = train_X, id = train_id, obsWeights = train_obsWeights, 
        ...)
    
    # validation objects
    valid_X <- validation(X)
    valid_Y <- validation(Y)
    valid_index <- validation()
    valid_obsWeights <- validation(obsWeights)
    valid_rows <- data.frame(index = valid_index, fold = fold_index())
    
    # get preds
    preds <- predict(o_SL, newdata = valid_X)
    library_names <- names(o_SL$cvRisk)
    all_preds <- abind(preds$pred, preds$library_pred, rev.along = 1)
    dimnames(all_preds)[[2]][1] <- "SuperLearner"
    
    # get risks
    cv_risks <- o_SL$fullFit$method$computeCoef(all_preds, valid_Y, dimnames(all_preds)[[2]], 
        F, valid_obsWeights, control = NULL)$cvRisk
    
    # format result
    list(preds = all_preds, true_y = valid_Y, valid_rows = valid_rows, cv_risks = as.data.frame(as.list(cv_risks)))
}

#' @export
origami_CV.SuperLearner <- function(Y, X, V = 10, id = NULL, obsWeights = rep(1, 
    length(Y)), ...) {
    
    folds <- make_folds(Y, cluster_ids = id, V = V)
    if (is.null(id)) {
        id <- seq_along(Y)
    }
    
    cv_results <- cross_validate(cv_cvsl, folds, Y, X, id, obsWeights, ...)
    
    # results
    out <- list(SL.predict = cv_results$preds, cvRisk = cv_results$cv_risks, folds = folds)
    class(out) <- c("origami_CV.SuperLearner")
    return(out)
}

#' @export
print.origami_CV.SuperLearner <- function(obj) {
    risk_df <- with(obj, data.frame(Ave = apply(cvRisk, 2, mean), se = apply(cvRisk, 
        2, sd)/sqrt(nrow(cvRisk)), Min = apply(cvRisk, 2, min), Max = apply(cvRisk, 
        2, max)))
    
    print(risk_df)
} 
