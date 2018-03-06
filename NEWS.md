# origami 1.0.0
* Adds a new function folds2foldvec for easy conversion between the folds
    structure used by origami and other packages (e.g., glmnet)
* Adds an option to use lapply instead of future_lapply, for compatibility with
    other packages in a custom parallelization framework
* Calls future_lapply from the new more modular package future.apply rather than
    from future, matching the author's recommendation

# origami 0.8.0
* First CRAN release.
* removed origami_SuperLearner functionality for now.
