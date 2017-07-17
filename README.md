
<!-- README.md is generated from README.Rmd. Please edit that file -->
R/`origami`
===========

[![Travis-CI Build Status](https://travis-ci.org/jeremyrcoyle/origami.svg?branch=master)](https://travis-ci.org/jeremyrcoyle/origami) [![Build status](https://ci.appveyor.com/api/projects/status/i5qwp8cjb4j4x329?svg=true)](https://ci.appveyor.com/project/jeremyrcoyle/origami) [![Coverage Status](https://img.shields.io/codecov/c/github/jeremyrcoyle/origami/master.svg)](https://codecov.io/github/jeremyrcoyle/origami?branch=master) [![CRAN](http://www.r-pkg.org/badges/version/origami)](http://www.r-pkg.org/pkg/origami) [![CRAN downloads](https://cranlogs.r-pkg.org/badges/origami)](https://CRAN.R-project.org/package=origami) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

> High-powered framework for cross-validation: fold your data like it's paper!

**Authors:** [Jeremy Coyle](https://github.com/jeremyrcoyle) and [Nima Hejazi](http://nimahejazi.org)

------------------------------------------------------------------------

Description
-----------

`origami` is an R package that provides a general framework for the application of cross-validation schemes to particular functions. By allowing arbitrary lists of results, `origami` accommodates a range of cross-validation applications.

------------------------------------------------------------------------

Installation
------------

For standard use, we recommend installing the package from [CRAN](https://cran.r-project.org/) via

``` r
install.packages("origami")
```

You can install a stable release of `origami` from GitHub via [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) with:

``` r
devtools::install_github("jeremyrcoyle/origami")
```

------------------------------------------------------------------------

Usage
-----

For details on how best to use `origami`, please consult the package [documentation](https://jeremyrcoyle.github.io/origami/) and [introductory vignette](https://jeremyrcoyle.github.io/origami/articles/generalizedCV.html) online, or do so from within [R](https://www.r-project.org/).

------------------------------------------------------------------------

Example
-------

This minimal example shows how to use `origami` to apply cross-validation to the computation of a simple descriptive statistic using a sample data set. In particular, we obtain a cross-validated estimate of the mean:

``` r
set.seed(4795)
library(stringr)
library(origami)
#> origami: Generalized Cross-Validation Framework
#> Version: 0.8.0

data(mtcars)
head(mtcars)
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

cv_lm <- function(fold, data, reg_form) {
  # get name and index of outcome variable from regression formula
  out_var <- as.character(unlist(str_split(reg_form, " "))[1])
  out_var_ind <- as.numeric(which(colnames(data) == out_var))

  # split up data into training and validation sets
  train_data <- training(data)
  valid_data <- validation(data)

  # fit linear model on training set and predict on validation set
  mod <- lm(as.formula(reg_form), data = train_data)
  preds <- predict(mod, newdata = valid_data)

  # capture results to be returned as output
  out <- list(coef = data.frame(t(coef(mod))),
              SE = ((preds - valid_data[, out_var_ind])^2))
  return(out)
}

folds <- make_folds(mtcars)
results <- cross_validate(cv_fun = cv_lm, folds = folds, data = mtcars,
                          reg_form = "mpg ~ .")
mean(results$SE)
#> [1] 15.22295
```

------------------------------------------------------------------------

Issues
------

If you encounter any bugs or have any specific feature requests, please [file an issue](https://github.com/jeremyrcoyle/origami/issues).

------------------------------------------------------------------------

Contributions
-------------

It is our hope that `origami` will grow to be adopted as a backend for most any procedure requiring cross-validation, including its integration into larger machine learning frameworks. To that end, contributions are very welcome, though we ask that interested contributors consult our [`contribution guidelines`](https://github.com/jeremyrcoyle/origami/blob/master/CONTRIBUTING.md) prior to submitting a pull request.

------------------------------------------------------------------------

Citation
--------

After using the `origami` R package, please cite it:

        @article{coyle2017origami,
          doi = {},
          url = {},
          year  = {2017},
          month = {},
          publisher = {The Open Journal},
          volume = {},
          number = {},
          author = {Coyle, Jeremy R and Hejazi, Nima S},
          title = {origami: A Generalized Framework for Cross-Validation in R},
          journal = {The Journal of Open Source Software}
        }

------------------------------------------------------------------------

License
-------

© 2017 [Jeremy R. Coyle](https://github.com/jeremyrcoyle)

The contents of this repository are distributed under the GPL-3 license. See file `LICENSE` for details.
