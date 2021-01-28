`%notin%` <- Negate(`%in%`)

fold_from_foldvec_subgroup <- function(v, foldsdf, n) {
  validation_set <- foldsdf[,1][which(foldsdf[,2] == v)]
  training_set <- which(seq_len(n) %notin% validation_set)
  make_fold(v, training_set, validation_set)
}

fold_from_foldvec_subgroup_stratoutcome <- function(v, foldsdf1, foldsdf2, n) {
  validation_set <- c(foldsdf1[,1][which(foldsdf1[,2] == v)], foldsdf2[,1][which(foldsdf2[,2] == v)])
  training_set <- which(seq_len(n) %notin% validation_set)
  make_fold(v, training_set, validation_set)
}

folds_subgroup <- function(n, V = 10L, subgroup, strat_outcome = NULL) {
  
  if(is.null(strat_outcome)==TRUE){
    
    subgroup_index <- which(subgroup == 1)
    
    #if (length(subgroup_index) <= V) {
    #  warning("number in subgroup <= V so using leave-one-out CV")
    #  return(folds_loo(subgroup_index))
    #}
    
    folds <- rep(seq_len(V), length = length(subgroup_index))
    
    # shuffle folds
    folds <- sample(folds)
    foldsdf <- data.frame(subgroup_index, folds)
    
    # generate fold vectors
    folds <- lapply(seq_len(V), fold_from_foldvec_subgroup, foldsdf, n)
    return(folds)
    
  }
  else {
    subgroup_index1 <- which(subgroup == 1 & strat_outcome == 1)
    subgroup_index2 <- which(subgroup == 1 & strat_outcome == 0)
    
    #if (length(subgroup_index) <= V) {
    #  warning("number in subgroup <= V so using leave-one-out CV")
    #  return(folds_loo(subgroup_index))
    #}
    
    #Two sets of folds, one for outcome == 1 and one for outcome == 0
    folds1 <- rep(seq_len(V), length = length(subgroup_index1))
    folds2 <- rep(seq_len(V), length = length(subgroup_index2))
    
    # shuffle folds
    folds1 <- sample(folds1)
    foldsdf1 <- data.frame(subgroup_index1, folds1)
    
    folds2 <- sample(folds2)
    foldsdf2 <- data.frame(subgroup_index2, folds2)
    
    # generate fold vectors
    folds <- lapply(seq_len(V), fold_from_foldvec_subgroup_stratoutcome, foldsdf1, foldsdf2,  n)
    return(folds)
  }
}