present <- function(x) {
    return(x)
}

# replicate foreach error collection
# function factory that generates wrapped version of functions
wrap_in_try <- function(fun, ...) {
    wrapped <- function(...)
    try({
        fun(...)
    }, silent = TRUE)
    
    return(wrapped)
}
