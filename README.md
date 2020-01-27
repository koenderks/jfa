<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/koenderks/jfa.svg?branch=master)](https://travis-ci.org/koenderks/jfa)
  [![Coverage](https://img.shields.io/codecov/c/github/koenderks/jfa)](https://codecov.io/gh/koenderks/jfa)
  [![Release](https://img.shields.io/github/v/release/koenderks/jfa?include_prereleases)](https://github.com/koenderks/jfa/releases)
  ![DownloadsMonthly](https://cranlogs.r-pkg.org/badges/jfa)
  ![DownloadsTotal](https://cranlogs.r-pkg.org/badges/grand-total/jfa)
  [![Issues](https://img.shields.io/github/issues/koenderks/jfa)](https://github.com/koenderks/jfa/issues)
<!-- badges: end -->

<p align="center">
  <img src="./man/figures/logo/jfaLogo.svg" align="middle" width="500.56" height="313.6">
</p>

# R package jfa

`jfa` is a multi-functional R package for statistical auditing. The package provides the user with four generic functions for planning, performing, and evaluating an audit and its results. Specifically, it contains functions for calculating sample sizes for substantive testing, sampling from data according to standard auditing techniques, and calculating various confidence bounds for the maximum error from data or summary statistics. The package also allows the user to create a Bayesian prior distribution for use in these functions. The `jfa` package can be used to set up the entire audit sampling workflow.

* [Installing](##Installing)  
* [Functions](##Functions) 
* [Poster](##Poster) 

For complete documentation, see the package [manual](./man/manual/jfa_0.1.0.pdf).

### Authors

* **Koen Derks** - *Initial work* - [Website](https://koenderks.com)

See also the list of [contributors](https://github.com/koenderks/jfa/graphs/contributors) who participated in this project.

### License

This project is licensed under the GPL-3 License.

## Installing

These instructions will get you a copy of the `jfa` package up and running on your 
local machine for use in R and RStudio. 

### Prerequisites

* [R](https://cran.r-project.org/mirrors.html) - The programming language used for deploying the package.

### Downloading

R package `jfa` is simple to download and set-up. The live version from [CRAN](https://cran.r-project.org/web/packages/jfa/index.html) can be downloaded by running the following command in R:

```
install.packages("jfa")
```

The `jfa` package can then be loaded in RStudio by typing:
```
library(jfa)
```

Examples can be found in the package [vignette](https://cran.r-project.org/web/packages/jfa/vignettes/auditWorkflow.html).

## Functions

Below is a list of the available functions in the current development version of `jfa`, sorted by their occurrence in the standard audit sampling workflow.

**auditPrior: Creating a prior distribution for substantive testing**

- `auditPrior()`

This function creates a prior distribution according to the audit risk model and assessments of the inherent and control risk. The returned object is of class `jfaPrior` and can be used with associated `print()` and `plot()` methods. `jfaPrior` results can also be used as input argument for the `prior` argument in other functions.

`auditPrior(materiality, confidence = 0.95, method = "arm", ir = 1, cr = 1, expectedError = 0, likelihood = "binomial", N = NULL)`

**Planning: Calculating an audit sample size**

- `planning()`

This function calculates the required sample size for an audit, based on the poisson, binomial, or hypergeometric likelihood. A prior can be specified to combine with the specified likelihood in order to perform Bayesian planning. The returned `jfaPlanning` object has a `print()` and a `plot()` method.

`planning(materiality = NULL, confidence = 0.95, expectedError = 0, distribution = "poisson" N = NULL, maxSize = 5000, prior = FALSE, kPrior = 0, nPrior = 0)`

**Sampling: Selecting transactions from a population**

- `sampling()`

This function takes a data frame and performs sampling according to one of three algorithms: random sampling, cell sampling, or fixed interval sampling in combination with either record sampling or monetary unit sampling. The returned `jfaSampling` object has a `print()` and a `plot()` method. The `sampleSize` argument can also be an object of class `jfaPlanning`.

`sampling(population, sampleSize, bookValues = NULL, algorithm = "random", units = "record", intervalStartingPoint = 1, ordered = TRUE, ascending = TRUE, withReplacement = FALSE, seed = 1)`

**Evaluation: Calculating confidence bounds for audit samples**

This function takes a sample data frame or summary statistics about an evaluated audit sample and calculates a confidence bound according to a specified method. The returned `jfaEvalution` object has a `print()` and `plot()` functions.

- `evaluation()`

`evaluation(sample = NULL, bookValues = NULL, auditValues = NULL, confidence = 0.95, dataType = "sample", sampleSize = NULL, sumErrors = NULL, method = "binomial", materiality = NULL, N = NULL, rohrbachDelta = 2.7)`

## Poster

<p align="center">
  <img src="./man/figures/poster/poster.png" align="middle">
</p>
