present <- function(x) {
    return(x)
}


# replicate foreach error collection
safe_eval <- function(fun, ...) {
    try({
        fun(...)
    }, silent = TRUE)
}
