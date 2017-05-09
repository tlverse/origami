make_interact_matrix=function(X,interact_var){
  form=as.formula(sprintf("~%s*.",interact_var))
  interact_X=model.matrix(form,data=X)[,-1]
  interact_X=as.data.frame(interact_X)
  names(interact_X)=gsub(":","_",names(interact_X))
  
  return(interact_X)
}

#' @export
wrapper_to_interact_wrapper <- function(wrapper, interact_var = "A") {
    # todo: add support for wrapper names
    if (is.character(wrapper)) {
        # print(wrapper)
        wrapper <- eval(parse(text = wrapper))
    }
    
    function(Y, X, newX, family, obsWeights, ...) {
        other_vars=setdiff(colnames(X),interact_var)  
        

        interact_X <- make_interact_matrix(X,interact_var)
        interact_newX <- make_interact_matrix(newX,interact_var)
        
        interact_fit=wrapper(Y, interact_X, interact_newX, family, obsWeights, ...)

        fit <- list(interact_fit = interact_fit, interact_var=interact_var)
        out <- list(pred = interact_fit$pred, fit = fit)
        
        
        class(out$fit) <- c("interactSL.wrapper")
        return(out)
    }
}

#' @export
predict.interactSL.wrapper <- function(object, newdata, ...) {
    interact_var <- object$interact_var
    interact_newdata <- make_interact_matrix(newdata,interact_var)

    preds <- predict(object$interact_fit$fit, interact_newdata, ...)
    return(preds)
}

#' @export
sl_to_interact_library <- function(SL.library, interact_var) {
    interactSL.library <- sprintf("interact%s", SL.library)
    for (i in seq_along(SL.library)) {
        assign(interactSL.library[i], wrapper_to_interact_wrapper(SL.library[i], interact_var), envir = globalenv())
    }
    
    return(interactSL.library)
}
