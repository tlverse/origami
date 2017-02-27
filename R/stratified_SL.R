#' @export
wrapper_to_strat_wrapper <- function(wrapper, strat_var = "A") {
    # todo: add support for wrapper names
    if (is.character(wrapper)) {
        # print(wrapper)
        wrapper <- eval(parse(text = wrapper))
    }
    
    function(Y, X, newX, family, obsWeights, ...) {
        strata <- X[, strat_var]
        X[, strat_var] <- NULL
        strat_levels <- unique(strata)
        newX_strata <- newX[, strat_var]
        newX[, strat_var] <- NULL
        
        
        col_fits <- lapply(strat_levels, function(strat_level) {
            strat_index <- which(strata == strat_level)
            new_strat_index <- which(newX_strata == strat_level)
            fit <- wrapper(index_dim(Y, strat_index), X[strat_index, ], newX[new_strat_index, ], family, index_dim(obsWeights, 
                strat_index), ...)
            fit$new_strat_index <- new_strat_index
            fit
        })
        
        col_preds <- unlist(lapply(col_fits, `[[`, "pred"))
        pred_index <- unlist(lapply(col_fits, `[[`, "new_strat_index"))
        preds <- index_dim(col_preds, order(pred_index))
        fit <- list(col_fits = col_fits, strat_levels = strat_levels, strat_var = strat_var)
        out <- list(pred = preds, fit = fit)
        
        
        class(out$fit) <- c("stratSL.wrapper")
        return(out)
    }
}

#' @export
predict.stratSL.wrapper <- function(object, newdata, ...) {
    strat_var <- object$strat_var
    strat_levels <- object$strat_levels
    newdata_strata <- newdata[, strat_var]
    newdata[, strat_var] <- NULL
    
    if (!all(newdata_strata %in% strat_levels)) {
        stop("Attempting to predict on unseen strata")
    }
    
    strat_nums_to_pred <- which(strat_levels %in% newdata_strata)
    col_preds <- lapply(strat_nums_to_pred, function(strat_num) {
        new_strat_index <- which(newdata_strata == strat_levels[strat_num])
        pred <- predict(object$col_fits[[strat_num]]$fit, newdata[new_strat_index, ], ...)
        list(pred = pred, new_strat_index = new_strat_index)
    })
    
    combined_col_preds <- unlist(lapply(col_preds, `[[`, "pred"))
    pred_index <- unlist(lapply(col_preds, `[[`, "new_strat_index"))
    preds <- index_dim(combined_col_preds, order(pred_index))
    return(preds)
}

#' @export
sl_to_strat_library <- function(SL.library, strat_var) {
    stratSL.library <- sprintf("strat%s", SL.library)
    for (i in seq_along(SL.library)) {
        assign(stratSL.library[i], wrapper_to_strat_wrapper(SL.library[i], strat_var), envir = globalenv())
    }
    
    return(stratSL.library)
}
