
cvfun <- function(split, aux = NULL, time = time) {
  code <- sprintf("t:%s s:%s", time, split$v)
  if (is.null(aux)) {
    sprintf("%s", code)
  } else {
    sprintf("%s - %s", code, aux[split$v])
  }
}

test1 <- function(splits, time) {
  if (time > 0) {
    pastresults <- test1(splits, time = time - 1)
    aux <- pastresults$splitResults
  } else {
    aux <- NULL
  }
  # t2
  splitResults <- genCV(cvfun, splits,
    aux = aux, .doPar = T,
    time = time
  )[[1]]
  results <- list(full = sprintf("t:%s full", time), splitResults = splitResults)
  return(results)
}

test1(splits, time = 2)

test2 <- function(splits, time) {
  if (time > 0) {
    pastresults <- test2(splits, time = time - 1)
    aux <- rep(pastresults$full, length(splits))
  } else {
    aux <- NULL
  }
  # t2
  splitResults <- genCV(cvfun, splits,
    aux = aux, .doPar = T,
    time = time
  )[[1]]
  results <- list(full = sprintf("t:%s full", time), splitResults = splitResults)
  return(results)
}

test2(splits, time = 2)

testonsplit <- function(split, time) {
  newsplits <- gensplits(split$test_idx)
  results <- test3(newsplits, time)
  return(results)
}

test3 <- function(splits, time) {
  if (time > 0) {
    pastSplitResults <- genCV(testonsplit, splits,
      .doPar = T,
      time = time - 1
    )
    aux <- pastSplitResults$full
    aux <- paste(aux, "nested")
  } else {
    aux <- NULL
  }
  # t2
  splitResults <- genCV(cvfun, splits,
    aux = aux, .doPar = T,
    time = time
  )[[1]]
  results <- list(full = sprintf("t:%s full", time), splitResults = splitResults)
  return(results)
}

test3(splits, time = 2)
