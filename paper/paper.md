---
title: "origami: A Generalized Framework for Cross-Validation in R"
tags:
  - "R"
  - "statistics"
  - "machine learning"
  - "cross-validation"
authors:
  - name: "Jeremy R. Coyle"
    orcid:
    affiliation: 1
  - name: "Nima S. Hejazi"
    orcid: 0000-0002-7127-2789
    affiliation: 1
affiliations:
  - name: "Division of Biostatistics, University of California, Berkeley"
    index: 1
date: "13 June 2017"
bibliography: paper.bib
---

## Introduction

Cross-validation is an essential tool for evaluating how any given data analytic
procedure extends from a sample to the target population from which the sample
is derived. It has seen widespread application in all facets of statistics,
perhaps most notably statistical machine learning.  When used for model
selection, cross-validation has powerful optimality properties
[@Vaart:2006bz, @vanderLaan:2007bz].

Cross-validation works by partitioning a sample into complementary subsets,
applying a particular data analytic (statistical) routine on a subset (the
"training" set), and evaluating the routine of choice on the complementary
subset (the "testing" set). This procedure is repeated across multiple
partitions of the data, and a variety of different partitioning schemes exist,
such as $V$-fold cross-validation and bootstrap cross-validation. `origami`, a
package for the R language for statistical computing [@R], supports many of the
existing cross-validation schemes, providing a suite of tools that generalize
the application of cross-validation to arbitrary data analytic procedures.

---

## General workflow

The main function in the `origami` R package is `cross_validate`. To start off,
the user must define folds and a function that operates on each fold. Once
these are passed to `cross_validate`, the function will map the function across
the folds, combining the results in a reasonable way. Specific details on each
each step of this process are given below.

### (1) Define folds

The `folds` object passed to `cross_validate` is a list of folds. Such lists can
be generated using the `make_folds` function. Each fold consists of a list
with a `training` index vector, a `validation` index vector, and a `fold_index`
(its order in the list of folds). This function supports a variety of
cross-validation schemes including $V$-fold and bootstrap cross-validation, as
well as time series methods like _"rolling window"_. See [@vanderLaan:2007bz]
for formal definitions of these schemes. `make_folds` can balance across levels
of a variable (`stratify_ids`), and it can also keep all observations from the
same independent unit together (`cluster`). We invite interested users to
consult the documentation of the `make_folds` function for further details.

### (2) Define fold function

The `cv_fun` argument to `cross_validate` is a function that will perform some
operation on each fold. The first argument to this function must be `fold`,
which will receive an individual fold object to operate on. Additional arguments
can be passed to `cv_fun` using the `...` argument to `cross_validate`. Within
this function, the convenience functions `training`, `validation` and
`fold_index` can return the various components of a fold object. If `training`
or `validation` is passed an object, it will index into it in a sensible way.
For instance, if it is a vector, it will index the vector directly. If it is a
`data.frame` or `matrix`, it will index rows. This allows the user to easily
partition data into training and validation sets. This fold function must return
a named list of results containing whatever fold-specific outputs are generated.

### (3) Apply `cross-validate`

After defining folds, `cross_validate` can be used to map the `cv_fun` across
the `folds` using `future_lapply`. This means that it can be easily parallelized
by specifying a parallelization scheme (i.e., a `plan`). See the [`future`
package](https://github.com/HenrikBengtsson/future) for more details.

The application of `cross_validate` generates a list of results. As described
above, each call to `cv_fun` itself returns a list of results, with different
elements for each type of result we care about. The main loop generates a list
of these individual lists of results (a sort of "meta-list"). This "meta-list"
is then inverted such that there is one element per result type (this too is a
list of the results for each fold). By default, `combine_results` is used to
combine these results type lists in a sensible manner. How results are combined
is determined automatically by examining the data types of the results from the
first fold. This can be modified by specifying a list of arguments to
`.combine_control`. See the help for `combine_results` for more details. In
most cases, the defaults should suffice.

Next, we demonstrate the use of `origami` by example.

---

## Cross-validation with linear regression

We'll start by examining a fairly simple data set:

```r
data(mtcars)
head(mtcars)
```

```
##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

One might be interested in examining how the efficiency of a car, as measured by
miles-per-gallon (mpg), is explained by various technical aspects of the car,
with data across a variety of different models of cars. Linear regression is
perhaps the simplest statistical procedure that could be used to make such
deductions. Let's try it out:

```r
mod <- lm(mpg ~ ., data = mtcars)
summary(mod)
```

```
## 
## Call:
## lm(formula = mpg ~ ., data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.4506 -1.6044 -0.1196  1.2193  4.6271 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) 12.30337   18.71788   0.657   0.5181
## cyl         -0.11144    1.04502  -0.107   0.9161
## disp         0.01334    0.01786   0.747   0.4635
## hp          -0.02148    0.02177  -0.987   0.3350
## drat         0.78711    1.63537   0.481   0.6353
## wt          -3.71530    1.89441  -1.961   0.0633
## qsec         0.82104    0.73084   1.123   0.2739
## vs           0.31776    2.10451   0.151   0.8814
## am           2.52023    2.05665   1.225   0.2340
## gear         0.65541    1.49326   0.439   0.6652
## carb        -0.19942    0.82875  -0.241   0.8122
## 
## Residual standard error: 2.65 on 21 degrees of freedom
## Multiple R-squared:  0.869,	Adjusted R-squared:  0.8066 
## F-statistic: 13.93 on 10 and 21 DF,  p-value: 3.793e-07
```

We can assess how well the model fits the data by comparing the predictions of
the linear model to the true outcomes observed in the data set. This is the
well known (and standard) squared error. We can extract that from the `lm` model
object like so:

```r
err <- mean(resid(mod)^2)
```

The squared error is 4.6092009. There is an important problem that arises when we
assess the model in this way -- that is, we have trained our linear regression
model on the full data set and assessed the error on the full data set, using up
all of our data. We, of course, are generally not interested in how well the
model explains variation in the observed data; rather, we are interested in
how the explanation provided by the model generalizes to a target population.
Having used all of our available data, we cannot honestly evaluate how well the
model fits (and thus explains) variation at the population level.

To resolve this issue, cross-validation allows for a particular procedure (e.g.,
linear regression) to be implemented over subsets of the data, evaluating how
well the procedure fits on a testing ("validation") set, thereby providing an
honest evaluation of the error.

We can easily add cross-validation to our linear regression procedure using
`origami`. First, let us define a new function to perform linear regression on a
specific partition of the data (called a "fold"):

```r
cvlm <- function(fold) {
    train_data <- training(mtcars)
    valid_data <- validation(mtcars)

    mod <- lm(mpg ~ ., data = train_data)
    preds <- predict(mod, newdata = valid_data)
    list(coef = data.frame(t(coef(mod))), SE = ((preds - valid_data$mpg)^2))
}
```

Our `cvlm` function is rather simple: we merely split the available data into
training and validation sets, using the eponymous `origami` functions, fit the
linear model on the training set, and evaluate the model on the testing set.
Having defined such a function, we can simply generate a set of partitions
using `origami`'s `make_folds` function, and apply our `cvlm` function over the
resultant `folds` object. Below, we replicate the resubstitution estimate of
the error -- we did this "by hand" above -- using the functions `make_folds`
and `cvlm`.


```r
library(origami)
```


```r
resub <- make_folds(mtcars, fold_fun = "resubstitution")[[1]]
resub_results <- cvlm(resub)
mean(resub_results$SE)
```

```
## [1] 4.609201
```

This (very nearly) matches the estimate of the error that we obtained above.

We can more honestly evaluate the error by $V$-fold cross-validation, which
partitions the data into $v$ subsets, fitting the model on $v - 1$ of the
subsets and evaluating on the held out subset. This is repeated such that each
subset is used for testing. We can easily apply our `cvlm` function using
`origami`'s `cross_validate` (n.b., by default this performs 10-fold
cross-validation):

```r
# cross-validated estimate
folds <- make_folds(mtcars)
results <- cross_validate(cvlm, folds)
mean(results$SE)
```

```
## [1] 10.73198
```

Having performed 10-fold cross-validation, we quickly notice that our previous
estimate of the model error (by resubstitution) was quite optimistic. The honest
estimate of the error is several times larger.

For further details about `origami`, please consult the vignette that
accompanies the R package.

---

\newpage

# References
