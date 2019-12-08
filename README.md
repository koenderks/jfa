<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/koenderks/jfa.svg?branch=master)](https://travis-ci.org/koenderks/jfa)
  [![Coverage](https://img.shields.io/codecov/c/github/koenderks/jfa)](https://codecov.io/gh/koenderks/jfa)
  [![Release](https://img.shields.io/github/v/release/koenderks/jfa?include_prereleases)](https://github.com/koenderks/jfa/releases)
  ![Downloads](https://img.shields.io/github/downloads/koenderks/jfa/total)
  [![Issues](https://img.shields.io/github/issues/koenderks/jfa)](https://github.com/koenderks/jfa/issues)
  ![Size](https://img.shields.io/github/repo-size/koenderks/jfa)
<!-- badges: end -->

<p align="center">
  <img src="./man/figures/logo/jfaLogo.svg" align="middle" width="271.56" height="313.6">
</p>

# R package jfa

`jfa` is a multi-functional R package for statistical auditing. The package provides the user with four generic functions for planning, performing, and evaluating an audit and its results. Specifically, it contains functions for calculating sample sizes for substantive testing, sampling from data according to standard auditing techniques, and calculating various confidence bounds for the maximum error from data or summary statistics. The package also allows the user to create a Bayesian prior distribution for use in these functions. The `jfa` package can be used to set up the entire audit sampling workflow.

* [Installing](##Installing)  
* [Functions](##Functions) 
* [Example](##Example) 
* [Poster](##Poster) 

### Authors

* **Koen Derks** - *Initial work* - [Website](https://koenderks.com)

See also the list of [contributors](https://github.com/koenderks/auditR/graphs/contributors) who participated in this project.

### License

This project is licensed under the GPL-3 License.

## Installing

These instructions will get you a copy of the `jfa` package up and running on your 
local machine for use in R and RStudio. 

### Prerequisites

* [R](https://cran.r-project.org/mirrors.html) - The programming language used for deploying the package.

### Downloading

R package `jfa` is simple to download and set-up. Untill there is a live version on [CRAN](https://cran.r-project.org/), the development version can be downloaded in the following manner:

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

This function takes a sample data frame or summary statistics about an evaluated audit sample and calculates a confidence bound accordint to a specified method. The returned `jfaEvalution` object has a `print()` and `plot()` functions.

- `evaluation()`

`evaluation(sample = NULL, bookValues = NULL, auditValues = NULL, confidence = 0.95, dataType = "sample", sampleSize = NULL, sumErrors = NULL, method = "binomial", materiality = NULL, N = NULL, rohrbachDelta = 2.7)`

## Example

Below is an example of how `jfa` may be used to facilitate the audit sampling workflow. The objective of this audit is to give a statement that, when 2.5 percent errors are found in the population of N = 1000, the auditor can state with 95 percent confidence that the misstatement in the population is lower than the materiality of 5 percent. In addition, the auditor has used the audit risk model to identify the inherent risk as 100 percent and the control risk as 60 percent. The auditor captures this information in the prior distribution and evaluates the population using the posterior distribution.

```
library(jfa)
set.seed(1)

# Generate some audit data (N = 1000).
population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), 
                         bookValue = runif(n = 1000, min = 700, max = 1000))

# Specify the materiality, confidence, and expected errors.
materiality   <- 0.05   # 5%
confidence    <- 0.95   # 95%
expectedError <- 0.025  # 2.5%

# Specify the inherent risk (ir) and control risk (cr).
ir <- 1     # 100%
cr <- 0.6   # 60%

# Create a beta prior distribution according to the Audit Risk Model (arm) and a binomial likelihood.
prior <- auditPrior(materiality = materiality, confidence = confidence, 
                    method = "arm", ir = ir, cr = cr, 
                    expectedError = expectedError, likelihood = "binomial")
print(prior)

# jfa prior distribution for arm method:
#      
# Prior sample size:     51 
# Prior errors:          1.27 
# Prior:                 beta(2.275, 50.725)

# Calculate the sample size according to the binomial distribution with the specified prior.
sampleSize <- planning(materiality = materiality, confidence = confidence, 
                      expectedError = expectedError, prior = prior, likelihood = "binomial")
print(sampleSize)

# jfa planning results for beta prior with binomial likelihood:
#      
# Materiality:             5% 
# Confidence:              95% 
# Sample size:             169 
# Allowed sample errors:   4.23 
# Prior parameter alpha:   2.275 
# Prior parameter beta:    50.725

# Draw sample using random monetary unit sampling.
sampleResult <- sampling(population = population, sampleSize = sampleSize, 
                         algorithm = "random", units = "mus", seed = 1, bookValues = "bookValue")
print(sampleResult)

# jfa sampling results for random monetary unit sampling: 
#      
# Population size:         1000 
# Sample size:             169 
# Proportion n/N:          0.169 
# Precentage of value:     16.84%

# Isolate the sample.
sample <- sampleResult$sample

# For this example, we use the book values as audit values. Implies zero errors at this point.
sample$trueValue <- sample$bookValue

# One overstatement is found.
sample$trueValue[2] <- sample$trueValue[2] - 500

# Evaluate the sample with one partial error using the posterior distribution.
conclusion <- evaluation(sample = sample, bookValues = "bookValue", auditValues = "trueValue", 
                         prior = prior, materiality = 0.05)
print(conclusion)

# jfa evaluation results for binomial likelihood with prior:
#   
# Materiality:           5% 
# Confidence:            95% 
# Upper bound:           2.729% 
# Sample size:           169 
# Sample errors:         1 
# Conclusion:            Approve population

# If you are curious...
plot(conclusion)
```

## Poster

<p align="center">
  <img src="./man/figures/poster/poster.png" align="middle">
</p>
