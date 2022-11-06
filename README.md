[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://github.com/koenderks/jfa/tree/development/R)
[![CRAN](https://img.shields.io/cran/v/jfa?color=yellow&label=CRAN&logo=r)](https://cran.r-project.org/package=jfa)
[![R_build_status](https://github.com/koenderks/jfa/workflows/Build/badge.svg)](https://github.com/koenderks/jfa/actions)
[![Codecov](https://codecov.io/gh/koenderks/jfa/branch/development/graph/badge.svg?token=ZoxIB8p8PW)](https://app.codecov.io/gh/koenderks/jfa)
[![Bugs](https://img.shields.io/github/issues/koenderks/jfa/bug?label=Bugs&logo=github&logoColor=%23FFF&color=brightgreen)](https://github.com/koenderks/jfa/issues?q=is%3Aopen+is%3Aissue+label%3Abug)
[![Total](https://cranlogs.r-pkg.org/badges/grand-total/jfa?color=blue)](https://cranlogs.r-pkg.org)

# jfa: Bayesian and Classical Methods for Auditing <img src='https://github.com/koenderks/jfa/raw/development/man/figures/logo.png' width='149' height='173' align='right'/>

`jfa` is an R package that provides Bayesian and classical statistical methods for audit sampling and data auditing. First, the package provides functions for planning, performing, evaluating, and reporting an audit sample compliant with international standards on auditing. Second, the package includes functions for auditing data, such as testing the distribution of first digits of a data set against Benford's law. For complete documentation of `jfa`, visit the [package website](https://koenderks.github.io/jfa/) or download the [package manual](https://cran.r-project.org/package=jfa/jfa.pdf).

- [Audit sampling: Get started](https://koenderks.github.io/jfa/articles/v1-audit-sampling.html)
- [Data auditing: Get started](https://koenderks.github.io/jfa/articles/v7-data-auditing.html)

The functionality of the `jfa` package and its intended workflow are also implemented with a graphical user interface in the [Audit](https://github.com/jasp-stats/jaspAudit) module of [JASP](https://jasp-stats.org), a free and open-source statistical software program.

## Installation

The most recently released version of `jfa` can be downloaded from [CRAN](https://cran.r-project.org/package=jfa) by running the following command in R:

```r
install.packages('jfa')
```

Alternatively, you can download the development version from GitHub using:

```r
# install.packages("remotes") # Uncomment if you do not have the 'remotes' package installed
remotes::install_github('koenderks/jfa')
```

After installation, the `jfa` package can be loaded with:

```r
library(jfa)
```

## Contributing

`jfa` is an open-source project that aims to be useful for the audit community. Your help in benchmarking and extending `jfa` is therefore greatly appreciated. Contributing to `jfa` does not have to take much time or knowledge, and there is extensive information available about it on the [Wiki](https://github.com/koenderks/jfa/wiki) of this repository.

If you are willing to contribute to the improvement of the package by adding a benchmark, please check out the Wiki page on [how to contribute a benchmark to jfa](https://github.com/koenderks/jfa/wiki/Benchmarks). If you are willing to contribute to the improvement of the package by adding a new statistical method, please check the Wiki page on [how to contribute a new method to jfa](https://github.com/koenderks/jfa/wiki/Methods).
