[![CRAN](https://img.shields.io/cran/v/jfa?color=yellow&label=CRAN&logo=r)](https://cran.r-project.org/package=jfa)
[![R_build_status](https://github.com/koenderks/jfa/workflows/Build/badge.svg)](https://github.com/koenderks/jfa/actions)
[![Codecov](https://codecov.io/gh/koenderks/jfa/branch/master/graph/badge.svg?token=ZoxIB8p8PW)](https://codecov.io/gh/koenderks/jfa)
[![Bugs](https://img.shields.io/github/issues/koenderks/jfa/bug?label=Bugs&logo=github&logoColor=%23FFF&color=brightgreen)](https://github.com/koenderks/jfa/issues?q=is%3Aopen+is%3Aissue+label%3Abug)
[![Monthly](https://cranlogs.r-pkg.org/badges/jfa?color=blue)](https://cranlogs.r-pkg.org)
[![Total](https://cranlogs.r-pkg.org/badges/grand-total/jfa?color=blue)](https://cranlogs.r-pkg.org)

# jfa: Bayesian and Classical Audit Sampling

<img src='https://github.com/koenderks/jfa/raw/master/man/figures/readme/logo/jfaLogo.png' width='149' height='173' alt='logo' align='right' margin-left='20' margin-right='20'/>

`jfa` is an R package for statistical audit sampling. The package provides five functions for planning, performing, evaluating, and reporting an audit sample. Specifically, these functions implement standard audit sampling techniques for calculating sample sizes, selecting items from a population, and evaluating the misstatement from a data sample or from summary statistics. Additionally, the `jfa` package allows the user to create a prior probability distribution to perform Bayesian audit sampling using these functions.

## Overview

For complete documentation of `jfa`, visit the [package website](https://koenderks.github.io/jfa/) or download the [package manual](https://cran.r-project.org/package=jfa/jfa.pdf).

1. [Installation](#1-installation)
2. [Cheat sheet](#2-cheat-sheet)
3. [Benchmarks](#3-benchmarks)
4. [Statistical tables](#4-statistical-tables)
5. [Available functions](#5-available-functions)
6. [References](#6-references)
7. [Package statistics](#7-package-statistics) 
8. [Contributing](#8-contributing) 

## 1. Installation

The most recently released version of `jfa` can be downloaded from [CRAN](https://cran.r-project.org/package=jfa) by running the following command in R:

```
install.packages('jfa')
```

Alternatively, you can download the development version from GitHub using:

```
devtools::install.github('koenderks/jfa')
```

After installation, the `jfa` package can be loaded with:

```
library(jfa)
```

## 2. Cheat sheet

The cheat sheet below can help you get started with the `jfa` package and its intended workflow. You can download a `pdf` version of the cheat sheet [here](https://github.com/koenderks/jfa/raw/master/man/figures/cheatsheet/cheatsheet.pdf).

<p align='center'><img src='https://github.com/koenderks/jfa/raw/master/man/figures/cheatsheet/cheatsheet.png' alt='cheatsheet' width='1000'></p>

## 3. Benchmarks

To validate the statistical results, `jfa`'s automated [unit tests](https://github.com/koenderks/jfa/tree/master/tests/testthat) regularly verify the main output from the package against the following benchmarks:

- [Audit Sampling: Audit Guide](https://future.aicpa.org/cpe-learning/publication/audit-sampling-audit-guide-OPL) (Appendix A and Appendix C)
- [AuditSampler](https://cplusglobal.wordpress.com/solutions/auditsampler-statistical-sampling-software/)
- [MUS](https://cran.r-project.org/package=MUS) (R package version 0.1.6)
- Touw, P., and Hoogduin, L. (2011). *Statistiek voor audit en controlling*. Boom uitgevers, Amsterdam. 
- [SMASH21 + SMASH21-Bayes](https://steekproeven.eu/)

## 4. Statistical tables

Below you can find several informative tables that contain statistical sample sizes, upper limits, and Bayes factors. These tables are created using the `planning()` and `evaluation()` functions provided in the package.

*Sample sizes*

- [Sample sizes based on the binomial distribution](https://github.com/koenderks/jfa/raw/master/man/figures/tables/jfaBinomialSampleSizes.pdf)
- [Sample sizes based on the Poisson distribution](https://github.com/koenderks/jfa/raw/master/man/figures/tables/jfaPoissonSampleSizes.pdf)
- [Sample sizes based on the hypergeometric distribution](https://github.com/koenderks/jfa/raw/master/man/figures/tables/jfaHypergeometricSampleSizes.pdf)

*Upper limits*

- [Upper limits based on the binomial distribution](https://github.com/koenderks/jfa/raw/master/man/figures/tables/jfaBinomialUpperBounds.pdf)
- [Upper limits based on the Poisson distribution](https://github.com/koenderks/jfa/raw/master/man/figures/tables/jfaPoissonUpperBounds.pdf)
- [Upper limits based on the hypergeometric distribution](https://github.com/koenderks/jfa/raw/master/man/figures/tables/jfaHypergeometricUpperBounds.pdf)

*Bayes factors*

- [Bayes factors based on the beta distribution](https://github.com/koenderks/jfa/raw/master/man/figures/tables/jfaBinomialBayesFactors.pdf)
- [Bayes factors based on the gamma distribution](https://github.com/koenderks/jfa/raw/master/man/figures/tables/jfaPoissonBayesFactors.pdf)
- [Bayes factors based on the beta-binomial distribution](https://github.com/koenderks/jfa/raw/master/man/figures/tables/jfaHypergeometricBayesFactors.pdf)

## 5. Available functions

<p align='center'><img src='https://github.com/koenderks/jfa/raw/master/man/figures/readme/banner/jfaBanner.png' alt='banner'/></p>

Below you can find an explanation of the available functions in `jfa`, sorted by their occurrence in the standard audit sampling workflow. For detailed examples of how to use these functions, visit the [Get started](https://koenderks.github.io/jfa/articles/jfa.html) section on the package website.

* [`auditPrior()`](#create-a-prior-distribution-with-the-auditprior-function)
* [`planning()`](#plan-a-sample-with-the-planning-function)
* [`selection()`](#select-items-with-the-selection-function)
* [`evaluation()`](#evaluate-a-sample-with-the-evaluation-function)
* [`report()`](#generate-a-report-with-the-report-function)

### Create a prior distribution with the `auditPrior()` function

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

The `auditPrior()` function creates a prior distribution according to one of several methods, including a translation of the assessments of the inherent risk and control risk from the audit risk model. The function returns an object of class `jfaPrior` which can be used with associated `print()` and `plot()` methods. Objects with class `jfaPrior` can also be used as input for the `prior` argument in other functions.

*Full function with default arguments:*

`auditPrior(confidence, materiality = NULL, expectedError = 0, method = 'none', likelihood = 'binomial', N = NULL, ir = 1, cr = 1, ub = NULL, pHmin = NULL, pHplus = NULL, sampleN = 0, sampleK = 0, factor = 1)`

*Supported options for the `method` argument:*

| `method` | Description | Required arguments | Reference |
| :----------- | :----------- | :----------- | :----------- |
| `none` | No prior information | | Derks et al. (2021) |
| `arm` | Translates risk assessments (ARM) | `ir` and `cr` | Derks et al. (2021) |
| `bram` | Bayesian risk assessment model (BRAM) | `ub` | Touw and Hoogduin (2011) |
| `median` | Equal prior probabilities for (in)tolerable misstatement | | Derks et al. (2021) |
| `hypotheses` | Custom prior probabilities for (in)tolerable misstatement | `pHmin` or `pHplus` | Derks et al. (2021) |
| `sample` | Earlier sample | `sampleN` and `sampleK` | Derks et al. (2021) |
| `factor` | Weighted earlier sample | `sampleN`, `sampleK`, and `factor` | Derks et al. (2021) |

*Supported options for the `likelihood` argument:*

| `likelihood` | Description | Reference |
| :----------- | :----------- | :----------- |
| `binomial` | Beta prior distribution (+ binomial likelihood) | Steele (1992) |
| `poisson` | Gamma prior distribution (+ Poisson likelihood) | Stewart (2013) |
| `hypergeometric` | Beta-binomial prior distribution (+ hypergeometric likelihood) | Dyer and Pierce (1991) |

### Plan a sample with the `planning()` function

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

The `planning()` function calculates the minimum sample size for a statistical audit sample based on the binomial, Poisson, or hypergeometric likelihood. The function returns an object of class `jfaPlanning` which can be used with associated `print()` and a `plot()` methods. To perform Bayesian planning, the input for the `prior` argument can be an object of class `jfaPrior` as returned by the `auditPrior()` function, or an object of class `jfaPosterior` as returned by the `evaluation()` function.

*Full function with default arguments:*

`planning(confidence, materiality = NULL, minPrecision = NULL, expectedError = 0, likelihood = 'binomial', N = NULL, prior = FALSE, nPrior = 0, kPrior = 0, increase = 1, maxSize = 5000)`

*Supported options for the `likelihood` argument:*

| `likelihood` | Description | Reference |
| :----------- | :----------- | :----------- |
| `binomial` | Binomial likelihood | Stewart (2012) |
| `poisson` | Poisson likelihood | Stewart (2012) |
| `hypergeometric` | Hypergeometric likelihood | Stewart (2012) |

### Select items with the `selection()` function

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

The `selection()` function takes a data frame and performs statistical sampling according to one of three algorithms: random sampling, cell sampling, or fixed interval sampling in combination with either record sampling or monetary unit sampling. The function returns an object of class `jfaSelection` which can be used with associated `print()` and a `plot()` methods. The input for the `sampleSize` argument can be an object of class `jfaPlanning` as returned by the `planning()` function.

*Full function with default arguments:*

`selection(population, sampleSize, units = 'records', algorithm = 'random', bookValues = NULL, intervalStartingPoint = 1, ordered = TRUE, ascending = TRUE, withReplacement = FALSE, seed = 1)`

*Supported options for the `units` argument:*

| `units` | Description | Required arguments |  Reference |
| :----------- | :----------- | :----------- | :----------- |
| `records` | Sampling units are items | |Leslie, Teitlebaum, and Anderson (1979) |
| `mus` | Sampling units are monetary units | `bookValues` | Leslie, Teitlebaum, and Anderson (1979) |

*Supported options for the `algorithm` argument:*

| `algorithm` | Description | Required arguments |
| :----------- | :----------- | :----------- |
| `random` | Select random units without the use of an interval | |
| `cell` | Select a random unit from every interval | |
| `interval` | Select a fixed unit from every interval | `intervalStartingPoint` |

### Evaluate a sample with the `evaluation()` function

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

The `evaluation()` function takes a sample or summary statistics of the sample and performs evaluation according to the specified method and sampling objectives. The function returns an object of class `jfaEvalution` which can be used with associated `print()` and `plot()` methods. To perform Bayesian evaluation, the input for the `prior` argument can be an object of class `jfaPrior` as returned by the `auditPrior()` function, or an object of class `jfaPosterior` as returned by the `evaluation()` function.

*Full function with default arguments:*

`evaluation(confidence, materiality = NULL, minPrecision = NULL, method = 'binomial', sample = NULL, bookValues = NULL, auditValues = NULL, counts = NULL, nSumstats = NULL, kSumstats = NULL, N = NULL, populationBookValue = NULL, prior = FALSE, nPrior = 0, kPrior = 0, rohrbachDelta = 2.7, momentPoptype = 'accounts', csA = 1, csB = 3, csMu = 0.5)`

*Supported options for the `method` argument:*

| `method` | Description | Required arguments | Reference |
| :----------- | :----------- | :----------- | :----------- |
| `binomial` | Binomial likelihood | | Stewart (2012) |
| `poisson` | Poisson likelihood | | Stewart (2012) |
| `hypergeometric` | Hypergeometric likelihood | | Stewart (2012) |
| `stringer` | Classical Stringer bound | | Bickel (1992) |
| `stringer-meikle` | Stringer bound with Meikle's correction | | Meikle (1972) |
| `stringer-lta` | Stringer bound with LTA correction | | Leslie, Teitlebaum, & Anderson (1979) |
| `stringer-pvz` | Modified Stringer bound | | Pap and van Zuijlen (1996) |
| `rohrbach` | Rohrbach's augmented variance estimator | `rohrbachDelta` | Rohrbach (1993) |
| `moment` | Modified moment bound | `momentPoptype` | Dworin and Grimlund (1984) |
| `coxsnell` | Cox and Snell bound | `csA`, `csB`, and `csMu` | Cox and Snell (1979) |
| `direct` | Direct estimator | `populationBookValue` | Touw and Hoogduin (2011) |
| `difference` | Difference estimator | `populationBookValue` | Touw and Hoogduin (2011) |
| `quotient` | Quotient estimator | `populationBookValue` | Touw and Hoogduin (2011) |
| `regression` | Regression estimator | `populationBookValue` | Touw and Hoogduin (2011) |

### Create a report with the `report()` function

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

The `report()` function takes an object of class `jfaEvaluation` as returned by the `evaluation()` function and automatically creates a `html` or `pdf` report containing the analysis results and their interpretation.

*Full function with default arguments:*

`report(object, file = 'report.html', format = 'html_document')`

For an example report, see the following [link](https://github.com/koenderks/jfa/raw/master/man/figures/readme/report/report.pdf).

## 6. References

- Bickel, P. J. (1992). Inference and auditing: The Stringer bound. *International Statistical Review*, 60(2), 197–209. - [View online](https://www.jstor.org/stable/1403650)
- Cox, D. R., & Snell, E. J. (1979). On sampling and the estimation of rare errors. *Biometrika*, 66(1), 125-132. - [View online](https://doi.org/10.1093/biomet/66.1.125)
- Derks, K. (2021). jfa: Bayesian and classical audit sampling. R package version 0.5.4. - [View online](https://cran.r-project.org/package=jfa)
- Derks, K., de Swart, J., van Batenburg, P., Wagenmakers, E.-J., & Wetzels, R. (2021). Priors in a Bayesian audit: How integration of existing information into the prior distribution can improve audit transparency and efficiency. *In Press*. - [View online](https://psyarxiv.com/8fhkp/)
- Dworin, L. D. and Grimlund, R. A. (1984). Dollar-unit sampling for accounts receivable and inventory. *The Accounting Review*, 59(2), 218–241. - [View online](https://www.jstor.org/stable/247296)
- Dyer, D., & Pierce, R. L. (1993). On the choice of the prior distribution in hypergeometric sampling. *Communications in Statistics - Theory and Methods*, 22(8), 2125-2146. - [View online](https://www.tandfonline.com/doi/abs/10.1080/03610929308831139)
- Meikle, G. R. (1972). *Statistical Sampling in an Audit Context*. Canadian Institute of Chartered Accountants.
- Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979). *Dollar-unit Sampling: A Practical Guide for Auditors*. London: Pitman.
- Pap, G., & van Zuijlen, M. C. (1996). On the asymptotic behaviour of the Stringer bound. *Statistica Neerlandica*, 50(3), 367-389. - [View online](https://doi.org/10.1111/j.1467-9574.1996.tb01503.x)
- Rohrbach, K. J. (1993). Variance augmentation to achieve nominal coverage probability in sampling from audit populations. *Auditing: A Journal of Practice & Theory*, 12(2), 79-97.
- Steele, A. (1992). *Audit Risk and Audit Evidence: The Bayesian Approach to Statistical Auditing*. San Diego: Academic Press.
- Stewart, T. R. (2012). *Technical Notes on the AICPA Audit Guide Audit Sampling*. American Institute of Certified Public Accountants, New York. - [View online](https://www.aicpa.org/content/dam/aicpa/publications/accountingauditing/keytopics/downloadabledocuments/sampling-guide-technical-notes.pdf)
- Stewart, T. R. (2013). *A Bayesian Audit Assurance Model with Application to the Component Materiality problem in Group Audits.* VU University, Amsterdam. - [View online](https://research.vu.nl/en/publications/a-bayesian-audit-assurance-model-with-application-to-the-componen)
- Talens, E. (2005). *Statistical Auditing and the AOQL-method*. University of Groningen, Groningen. - [View online](https://research.rug.nl/en/publications/statistical-auditing-and-the-aoql-method)
- Touw, P., and Hoogduin, L. (2011). *Statistiek voor Audit en Controlling*. Boom uitgevers, Amsterdam.

## 7. Package statistics

<img src='https://github.com/koenderks/jfa/raw/master/man/figures/readme/downloads/downloads.svg' width='50%' /><img src='https://github.com/koenderks/jfa/raw/master/man/figures/readme/worldmap/worldmap.svg' width='50%' />

## 8. Contributing

`jfa` is an open-source project that aims to be useful for the audit community. Your help in benchmarking and extending `jfa` is therefore greatly appreciated. Contributing to `jfa` does not have to take much time or knowledge, and there is extensive information available about it on the [Wiki](https://github.com/koenderks/jfa/wiki) of this repository.

If you are willing to contribute to the improvement of the package by adding a benchmark, please check out the Wiki page on [how to contribute a benchmark to jfa](https://github.com/koenderks/jfa/wiki/Benchmarks). If you are willing to contribute to the improvement of the package by adding a new statistical method, please check the Wiki page on [how to contribute a new method to jfa](https://github.com/koenderks/jfa/wiki/Methods).