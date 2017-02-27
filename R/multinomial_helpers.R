#' @export
vals_from_factor <- function(x) {
    # lev <- levels(as.factor(x)) factor(lev, levels = lev)
    sort(unique(x))
}

#' @export
factor_to_indicators <- function(x) {
    x_vals <- vals_from_factor(x)
    ind_mat <- sapply(x_vals, function(x_val) as.numeric(x_val == x))
    colnames(ind_mat) <- x_vals
    
    return(ind_mat)
}

#' @export
pred_all_Q <- function(Q_fit, newdata, A_vals, Anode) {
    sapply(A_vals, function(A_val) {
        newdata[, Anode] <- A_val
        predict(Q_fit, newdata)$pred
    })
    
}

# make each row sum to 1
#' @export
normalize_rows <- function(x) {
    sweep(x, 1, rowSums(x), "/")
}
