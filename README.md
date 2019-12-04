<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/koenderks/jfa.svg?branch=master)](https://travis-ci.org/koenderks/jfa)
  [![Coverage](https://img.shields.io/codecov/c/github/koenderks/jfa)](https://codecov.io/gh/koenderks/jfa)
  [![Release](https://img.shields.io/github/v/release/koenderks/jfa)](https://github.com/koenderks/jfa/releases)
  ![Downloads](https://img.shields.io/github/downloads/koenderks/jfa/total)
  [![Issues](https://img.shields.io/github/issues/koenderks/jfa)](https://github.com/koenderks/jfa/issues)
  ![Size](https://img.shields.io/github/repo-size/koenderks/jfa)
<!-- badges: end -->

<p align="center">
  <img src="./man/figures/logo/jfaLogo.svg" align="middle" width="271.56" height="313.6">
</p>

# R package jfa

`jfa` is a multifunctional R package for statistical auditing. The package provides the user with three generic functions for planning, 
performing, and evaluating an audit and its results. Specifically, it contains functions for calculating sample sizes for 
substantive testing, sampling from data according to standard auditing techniques, and calculating various confidence bounds 
for the maximum error from data or summary statistics. The `jfa` package can be used to set up the entire audit sampling 
workflow.

## Getting Started

These instructions will get you a copy of the `jfa` package up and running on your 
local machine for use in R and RStudio. 

### Prerequisites

* [R](https://cran.r-project.org/mirrors.html) - The programming language used
* [RStudio](https://www.rstudio.com/products/rstudio/download/) - Integrated Development Environment (IDE)

### Installing

R package `jfa` is simple to download and set-up. Untill there is a live version
on [CRAN](https://cran.r-project.org/), the development version can be downloaded
in the following manner:

The package you will need for this is the `devtools` package. You can obtain this package by running
the following command in the R or RStudio console (provided you have a working internet connection):

```
install.packages("devtools")
```

Once the `devtools` package is installed, the only thing required to obtain `jfa` is 
installing the source package from this github page with the following command:

```
devtools::install_github("koenderks/jfa", INSTALL_opts=c("--no-multiarch"))
```

The `jfa` package can then be loaded in RStudio by typing:
```
library(jfa)
```

### Authors

* **Koen Derks** - *Initial work* - [Website](https://koenderks.com)

See also the list of [contributors](https://github.com/koenderks/auditR/graphs/contributors) who participated in this project.

### License

This project is licensed under the GPL-3 License.

### Available Functions

Below is a list of the available functions in the current development version of `jfa`, sorted by their occurrence in the standard audit sampling workflow.

**Planning: Calculating an audit sample size**

- `planning()`

This function calculates the required sample size for an audit, based on the poisson, binomial, or hypergeometric likelihood. A prior can be specified to combine with the specified likelihood in order to perform Bayesian planning. The returned `jfaPlanning` object has a `print()` and a `plot()` method.

`planning(materiality = NULL, confidence = 0.95, expectedError = 0, distribution = "poisson" N = NULL, maxSize = 5000, prior = FALSE, kPrior = 0, nPrior = 0)`

**Sampling: Selecting transactions from a population**

- `sampling()`

This function takes a data frame and performs sampling according to one of three algorithms: random sampling, cell sampling, or fixed interval sampling in combination with either record sampling or monetary unit sampling. The returned `jfaSampling` object has a `print()` and a `plot()` method. The `sampleSize` argument can also be an object of class `jfaPlanning`.

`sampling(population, sampleSize, bookValues = NULL, algorithm = "random", units = "record", intervalStartingPoint = 1, ordered = TRUE, ascending = TRUE, withReplacement = FALSE, seed = 1)`

**Evaluation: Calculating confidence bounds for audit samples**

This function takes a sample data frame or summary statistics about an evaluated audit sample and calculates a confidence bound accordint to a specified method. The returned `jfaEvalution` object has a `print()` and `plot()` functions.

- `evaluation()`

`evaluation(sample = NULL, bookValues = NULL, auditValues = NULL, confidence = 0.95, dataType = "sample", sampleSize = NULL, sumErrors = NULL, method = "binomial", materiality = NULL, N = NULL, rohrbachDelta = 2.7)`

### The audit sampling workflow

```
library(jfa)

# Generate some audit data (N = 1000)
set.seed(1)
data <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), 
                   bookValue = runif(n = 1000, min = 1000, max = 10000))

# Using the binomial likelihood, calculates the upper 95% confidence bound for a 
# materiality of 5% when 1% full errors are found in a sample (n = 93).
jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.01, 
                   likelihood = "binomial")

# Using monetary unit sampling, draw a random sample from the population.
samp <- sampling(population = data, sampleSize = jfaRes, 
                 units = "mus", bookValues = "bookValue", algorithm = "random")

samp$sample$trueValue <- samp$sample$bookValue
samp$sample$trueValue[2] <- 1561.871 - 500 # One overstatement is found

# Evaluate the sample using the Stringer bound.
evaluation(sample = samp$sample, bookValues = "bookValue", 
           auditValues = "trueValue", method = "stringer", materiality = 0.05)

# jfa results for evaluation with stringer method
#   
# Materiality:           5% 
# Confidence:            95% 
# Upper bound:           3.756% 
# Sample size:           93 
# Sample errors:         1 
# Conclusion:            Approve population
```
