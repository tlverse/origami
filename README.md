
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R/`origami` <img src="./hex/origami-sticker.png" align="right" width='125'/>

[![R-CMD-check](https://github.com/tlverse/origami/workflows/R-CMD-check/badge.svg)](https://github.com/tlverse/origami/actions)
[![Coverage
Status](https://codecov.io/gh/tlverse/origami/branch/master/graph/badge.svg)](https://codecov.io/gh/tlverse/origami)
[![CRAN](http://www.r-pkg.org/badges/version/origami)](http://www.r-pkg.org/pkg/origami)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/origami)](https://CRAN.R-project.org/package=origami)
[![CRAN total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/origami)](https://CRAN.R-project.org/package=origami)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1155901.svg)](https://doi.org/10.5281/zenodo.1155901)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00512/status.svg)](https://doi.org/10.21105/joss.00512)

> High-powered framework for cross-validation. Fold your data like it’s
> paper\!

**Authors:** [Jeremy Coyle](https://github.com/jeremyrcoyle), [Nima
Hejazi](https://nimahejazi.org), [Ivana
Malenica](https://github.com/podTockom), and [Rachael
Phillips](https://github.com/rachaelvphillips)

-----

## What’s `origami`?

The `origami` R package provides a general framework for the application
of cross-validation schemes to particular functions. By allowing
arbitrary lists of results, `origami` accommodates a range of
cross-validation applications.

-----

## Installation

For standard use, we recommend installing the package from
[CRAN](https://cran.r-project.org/) via

``` r
install.packages("origami")
```

You can install a stable release of `origami` from GitHub via
[`devtools`](https://www.rstudio.com/products/rpackages/devtools/) with:

``` r
devtools::install_github("tlverse/origami")
```

-----

## Usage

For details on how best to use `origami`, please consult the package
[documentation](https://origami.tlverse.org) and [introductory
vignette](https://origami.tlverse.org/articles/generalizedCV.html)
online, or do so from within [R](https://www.r-project.org/).

-----

## Example

This minimal example shows how to use `origami` to apply
cross-validation to the computation of a simple descriptive statistic
using a sample data set. In particular, we obtain a cross-validated
estimate of the mean:

``` r
library(stringr)
library(origami)
#> origami v1.0.4: Generalized Framework for Cross-Validation
set.seed(4795)

data(mtcars)
head(mtcars)
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
# build a cv_fun that wraps around lm
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
#> [1] 15.18558
```

For details on how to write wrappers (`cv_fun`s) for use with
`origami::cross_validate`, please consult the documentation and
vignettes that accompany the package.

-----

## Issues

If you encounter any bugs or have any specific feature requests, please
[file an issue](https://github.com/tlverse/origami/issues).

-----

## Contributions

Contributions are very welcome. Interested contributors should consult
our [contribution
guidelines](https://github.com/tlverse/origami/blob/master/CONTRIBUTING.md)
prior to submitting a pull request.

-----

## Citation

After using the `origami` R package, please cite it:

``` 
    @article{coyle2018origami,
      author = {Coyle, Jeremy R and Hejazi, Nima S},
      title = {origami: A Generalized Framework for Cross-Validation in R},
      journal = {The Journal of Open Source Software},
      volume = {3},
      number = {21},
      month = {January},
      year  = {2018},
      publisher = {The Open Journal},
      doi = {10.21105/joss.00512},
      url = {https://doi.org/10.21105/joss.00512}
    }
```

-----

## License

© 2017-2021 [Jeremy R. Coyle](https://github.com/jeremyrcoyle)

The contents of this repository are distributed under the GPL-3 license.
See file `LICENSE` for details.
