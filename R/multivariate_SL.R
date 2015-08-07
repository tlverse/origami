#' @export
wrapper_to_mv_wrapper <- function(wrapper) {
    # todo: add support for wrapper names
    if (is.character(wrapper)) {
        # print(wrapper)
        wrapper <- eval(parse(text = wrapper))
    }
    
    function(Y, ...) {
        nY <- ncol(Y)
        col_fits <- lapply(seq_len(nY), function(col) {
            wrapper(Y[, col], ...)
        })
        
        col_preds <- sapply(col_fits, `[[`, "pred")
        fit <- list(col_fits = col_fits)
        out <- list(pred = col_preds, fit = fit)
        
        
        class(out$fit) <- c("mvSL.wrapper")
        return(out)
    }
}

#' @export
predict.mvSL.wrapper <- function(object, ...) {
    col_preds <- sapply(object$col_fits, function(x) predict(x$fit, ...))
    return(col_preds)
}

#' @export
# coefs are a simple average of the per-Y coeffcients todo: actually maximize in
# the mv space
method.mvSL <- function(base_method) {
    out <- list(require = NULL, computeCoef = function(Z, Y, libraryNames, verbose, 
        obsWeights, ...) {
        nY <- ncol(Y)
        
        col_coef <- lapply(seq_len(nY), function(i) {
            base_method$computeCoef(Z[, i, ], Y[, i], libraryNames, verbose, obsWeights, 
                ...)
        })
        
        cvRisk <- rowMeans(sapply(col_coef, `[[`, "cvRisk"))
        
        # get init coef estimate <- average of per column estimates
        coef <- rowMeans(sapply(col_coef, `[[`, "coef"))
        
        # get optimum coef
        risk_fun <- function(coef) {
            normcoef <- normalize(coef)
            
            risks <- sapply(seq_len(nY), function(i) {
                pred <- base_method$computePred(Z[, i, ], normcoef)
                base_method$computeCoef(pred, Y[, i], "combination", verbose, obsWeights, 
                  ...)$cvRisk
            })
            mean(risks)
        }
        
        opt <- optim(risk_fun, par = coef, method = "L-BFGS-B", lower = 0, control = list(trace = FALSE))
        coef <- normalize(opt$par)
        
        out <- list(cvRisk = cvRisk, coef = coef)
        return(out)
    }, computePred = function(predY, coef, ...) {
        out <- apply(predY, 2, base_method$computePred, coef, ...)
        
        return(out)
    })
    invisible(out)
}

#' @export
sl_to_mv_library <- function(SL.library) {
    mvSL.library <- sprintf("mv%s", SL.library)
    for (i in seq_along(SL.library)) {
        assign(mvSL.library[i], wrapper_to_mv_wrapper(SL.library[i]), envir = globalenv())
    }
    
    return(mvSL.library)
}

# mvLibrary=sl_to_mv_library(SL.library$QaV)
# sapply(mvLibrary,function(x){get('wrapper',envir=environment(eval(parse(text=x))))})
# predictions=val_preds test=val_preds$QaW

# head(test) #debug(mvSL.mean) debug(predict.mvSL.wrapper) method=method.NNLS()
# base_method=method.NNLS() mv.method=method.mvSL(base_method)
# #debug(mv.method$computeCoef)
# test=origami_SuperLearner(folds=folds,Y=val_preds$QaW,X=data[,nodes$Wnodes],SL.library=mvLibrary,method=mv.method) 
