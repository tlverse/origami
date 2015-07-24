#' @title drop_zero_learners
#' @description Drops library learners that were assigned 0 weight, speeding up future predictions.
#' @param osl_fit fit generated using orgami_SuperLearner.
#' @param osl_fit orgami_SuperLearner without the zero weight learners.
#' @export
drop_zero_learners <- function(osl_fit) {
    coef <- osl_fit$coef
    is_nz <- which(coef != 0)
    coefnz <- coef[is_nz]
    osl_fit$coef <- coefnz
    last_dim <- length(safe_dim(osl_fit$Z))
    osl_fit$Z <- index_dim(osl_fit$Z, is_nz, last_dim)
    osl_fit$fullFit$coef <- coefnz
    osl_fit$fullFit$library_fits <- osl_fit$fullFit$library_fits[is_nz]
    
    for (i in seq_along(osl_fit$foldsFits)) {
        osl_fit$foldFits[[i]]$coef <- coefnz
        osl_fit$foldFits[[i]]$library <- osl_fit$foldFits[i]$library[is_nz]
    }
    return(osl_fit)
} 
