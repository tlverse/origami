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


# sort n-dimensional array (for multinomial/multivariate SL support)
aorder <- function(mat, index, along = 1) {
    
    dims <- safe_dim(mat)
    args <- ifelse(along == seq_along(dims), "index", "")
    indexer <- paste(c(args, "drop=F"), collapse = ",")
    call <- sprintf("mat[%s]", indexer)
    result <- eval(parse(text = call))
    
    return(result)
} 
