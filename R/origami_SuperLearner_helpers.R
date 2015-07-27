
fit_drop_zero_learners <- function(osl_fit) {
    coef <- osl_fit$coef
    is_nz <- which(coef != 0)
    if (length(is_nz) == 1) {
        # make sure we have at least two learners (so we don't break the combination
        # methods)
        is_nz <- c(is_nz, length(coef))
    }
    coefnz <- coef[is_nz]
    osl_fit$coef <- coefnz
    osl_fit$library_fits <- osl_fit$library_fits[is_nz, drop = FALSE]
    
    return(osl_fit)
}

#' @title drop_zero_learners
#' @description Drops library learners that were assigned 0 weight, speeding up future predictions.
#' @param osl_obj object generated using orgami_SuperLearner.
#' @return osl_obj orgami_SuperLearner without the zero weight learners.
#' @export
drop_zero_learners <- function(osl_obj) {
    osl_obj$fullFit <- fit_drop_zero_learners(osl_obj$fullFit)
    
    for (i in seq_along(osl_obj$foldFits)) {
        osl_obj$foldFits[[i]] <- fit_drop_zero_learners(osl_obj$foldFits[[i]])
    }
    return(osl_obj)
} 
