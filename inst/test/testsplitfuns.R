strataids <- rep(1:2, 20)
n <- length(strataids)
folds <- make_folds(strataids = strataids)
lapply(folds, function(fold) {
  table(strataids, seq_len(n) %in%
    training(fold = fold))
})

clusterids <- rep(1:20, 2)
n <- length(clusterids)
folds <- make_folds(clusterids = clusterids)
lapply(folds, function(fold) {
  table(clusterids, seq_len(n) %in%
    training(fold = fold))
})

folds <- make_folds(V = 2, strataids = strataids, clusterids = clusterids)

lapply(folds, function(fold) {
  table(strataids, clusterids, seq_len(n) %in%
    training(fold = fold))
})
