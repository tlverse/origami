# origami 1.0.3
* A maintenance release addressing reported issues, including changes to unit
   tests that relied on Suggested packages dependencies.

# origami 1.0.2
* Adds new functionality for cross-validation with time-series, especially
   functionality for pooling time-series data. This includes the new functions
   `folds_rolling_origin_pooled`, `folds_rolling_window_pooled`,
   `folds_vfold_rolling_origin_pooled`, `folds_vfold_rolling_window_pooled`.

# origami 1.0.1
* Adds a simple check of the current R version in the unit tests in the file
   tests/testthat/test-overall.R to use appropriate hard-coded values that
   depend on the R version. Note that these differ between R < 3.6.0 and
   R > 3.6.0 due to an important change in the default PRNG.

# origami 1.0.0
* Adds a new function folds2foldvec for easy conversion between the folds
    structure used by origami and other packages (e.g., glmnet).
* Adds an option to use lapply instead of future_lapply, for compatibility with
    other packages in a custom parallelization framework.
* Calls future_lapply from the new more modular package future.apply rather than
    from future, matching the author's recommendation.

# origami 0.8.0
* First CRAN release.
* Removed origami_SuperLearner functionality temporarily.
