message_verbose <- function(msg, msglevel, verbosity) {
    if (msglevel <= verbosity) {
        message(msg)
    }
}

# dim that works for vectors too
safe_dim <- function(x) {
    d <- dim(x)
    if (is.null(d)) {
        d <- length(x)
    }
    
    return(d)
}

# pull indexed values out of object with arbitrary dimensions
index_dim <- function(x, indicies, which_dim = 1, drop = FALSE) {
    ndim <- length(safe_dim(x))
    drop_text <- sprintf("drop=%s", drop)
    index_text <- c(rep("", ndim), drop_text)
    index_text[which_dim] <- "indicies"
    command <- sprintf("x[%s]", paste(index_text, collapse = ","))
    
    eval(parse(text = command))
} 
