# jfa

A multifunctional R package for auditing based on the methods in JASP for Audit. 
The package provides functions for  planning, performing and evaluating an audit and its results. 
More specific, it contains functions for calculating sample sizes for substantial testing, sampling
from data according to standard auditing techniques and calculating various 
confidence bounds for the maximum error.

## Getting Started

These instructions will get you a copy of the package up and running on your 
local machine for use in R and RStudio. 

### Prerequisites

* [R](https://cran.r-project.org/mirrors.html) - The programming language used
* [RStudio](https://www.rstudio.com/products/rstudio/download/) - Integrated 
Development Environment (IDE)

### Installing

R package jfa is simple to download and set-up. Untill there is a live version
on [CRAN](https://cran.r-project.org/), the development version can be downloaded
in the following manner:

The package we need is the `devtools` package. You can obtain this package by running
the following command in the R or RStudio console:

```
install.packages("devtools")
```

Once the package is installed, the only thing required to obtain `auditR` is 
installing the source package with the following command:

```
devtools::install_github("koenderks/jfa")
```

The `auditR` package can then be loaded in RStudio by typing:
```
library(auditR)
```

### Authors

* **Koen Derks** - *Initial work* - [Website](https://koenderks.com)

See also the list of [contributors](https://github.com/koenderks/auditR/graphs/contributors) who participated in this project.

### License

This project is licensed under the GPL-3 License.

### Available Functions

Below is a list of the available functions in the current development version of
auditR, sorted by purpose of use.

**Sample Size Calculations**

- `calc.n.binomial()`
- `calc.n.hypergeometric()`
- `calc.n.beta()`

**Sampling Methods**

- `random.sampling()`

**Confidence Bounds**

- `attributes.bound()`
- `normal.bound()`
- `stringer.bound()`
- `stringer.meikle()`
- `stringer.lta()`
- `stinger.bickel()`
- `stringer.modified()`
- `modified.moment()`
- `cox.snell()`
- `rohrbach.bound()`
