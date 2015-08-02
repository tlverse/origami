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

#' @export
split_fold <- function(fold, splits = 2) {
    v <- fold_index()
    train_idx <- training()
    valid_idx <- validation()
    val_folds <- make_folds(valid_idx, V = splits)
    val_fold <- val_folds[[1]]
    make_split(val_fold)
    make_split <- function(val_fold) {
        list(split_fold = make_fold(v, train_idx, validation(valid_idx, fold = val_fold)))
    }
    split_folds <- cross_validate(make_split, val_folds)$split_fold
    names(split_folds) <- sprintf("split_%d", seq_len(splits))
    
    return(split_folds)
}

#' @export
make_split_folds <- function(folds, splits = 2) {
    result <- cross_validate(split_fold, folds, splits = splits)
    
    return(result)
}

refit_origami_SuperLearner <- function(split_fold, osl_obj) {
    
} 
