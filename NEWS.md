# jfa 0.5.8

**New features**

- Added `method = 'custom'` to function `auditPrior()` which takes as input the raw `alpha` and `beta` parameters of the prior distribution.
- Added a new vignette that describes the sampling methodology implemented in `jfa`.

**Minor changes**

- Changed the name of the `withReplacement` argument in the `selection()` function to `replace` to be consistent with general `R` style.
- It is now allowed for `nSumstats` and `kSumstats` to have the same value.

# jfa 0.5.7

**Minor changes**

- The logo is now displayed in the `?jfa-package` help file.
- The cheat sheet link has changed in the README file.

# jfa 0.5.6

**Bug fixes**

- Fixed a bug in the `print.jfaEvaluation()` call if there was no performance materiality specified and `prior = TRUE`.

# jfa 0.5.5

**New features**

- The `print()` functions now return a more concise description of the relevant output.
- Added `summary()` functions for all returned objects that take over the former (elaborate) output of the `print()` functions.
- Implemented a new function `auditBF()` which computes Bayes factors from summary statistics of an audit sample.

**Bug fixes**

- Fixed a bug in `evaluation()` in which the likelihood stored in the prior was not properly passed to the function.
- Fixed an error in the calculation of the posterior mode of the beta distribution.

**Minor changes**

- Restored the default value (0.95) for the 'confidence' argument in all applicable functions.

# jfa 0.5.4

**New features**

- Objects with class `jfaPosterior` as returned by `evaluation()$posterior` and `planning()$expectedPosterior` can now be used as input for the `prior` argument in the `planning()` and `evaluation()` functions.

**Bug fixes**

- Fixed a bug in `method = 'bram'` in the `auditPrior()` function where the prior parameters would go off to infinity when `expectedError = 0`.

**Major changes**

- Now calculates the upper bound for the population errors according to the hypergeometric distribution via an inverted hypothesis test. As a result of this method, the `planning()` function does not require a value for the `materiality` anymore when planning with the `hypergeometric` likelihood.

**Minor changes**

- Added a benchmark for the `MUS` package to the unit tests.
- Improved plots with better titles and axes labels.

# jfa 0.5.3

**New features**

- Made `expectedErrors > 0` available for `method = 'hypotheses'` in the `auditPrior()` function.
- Made `method = 'hypotheses'` and `method = 'median'` in the `auditPrior()` function available for `likelihood = 'hypergeometric'`.
- Added `bram` as a method for the `auditPrior()` function. `method = 'bram'` computes a prior distribution with a given mode (`expectedError`) and upper bound (`ub`).

**Bug fixes**

- Fixed an error in the mode of the gamma posterior distribution from the `evaluation()` function in which `+1` was added to the beta parameter, resulting in slightly lower modes than the correct ones.
- Made a correction to the calculation of the beta-binomial prior and posterior so that the posterior parameter `N` has the correct value of `N = N - n` (current) instead of `N - n + k` (before).

**Major changes**

- Removed the default value `confidence = 0.95` in all applicable functions. `confidence` currently has no default value so that the user is required to give an input.
- Changed the default `likelihood = 'poisson'` in the `planning()` function to `likelihood = 'binomial'` to be consistent across all functions.
- Changed the order of most function arguments so that `materiality` and `minPrecision` are among the first ones to be shown.

**Minor changes**

- Updated the documentation for all functions with more simple examples.

# jfa 0.5.2

**New features**

- Update the poisson evaluation calculation so that it allows for fractional errors.

**Bug fixes**

- Fixed an error in the hypergeometric upper bound calculation that was accidentally based on the `phyper()` function instead of the `qhyper()` function, which resulted in lower bounds than usual.

**Minor changes**

- Add statistical tables with output (sample sizes, upper limits, Bayes factors) to the GitHub repository in pdf format.
- Changed the computation method of the sample sizes for hypergeometric and beta-binomial distributions so that they are faster.

# jfa 0.5.1

**Bug fixes**

- Reduced the size of the tarball by adding files to the .Rbuildignore
- Fixed a bug in `selection()` where if `population` is sorted or modified, `bv` still retained the old ordering and data. The resulting sample was overweighted towards small values and/or still contained negative values (Thanks to @alvanson).

# jfa 0.5.0

**New features**

- Add a function `report()` that automatically generates an audit report.

**Major changes**

- Removed the `sampling()` function, which is now replaced entirely with the `selection()` function.
- Changed the output of the `evaluation()` function when an estimator is used.

# jfa 0.4.0

**New features**

- Added `digits` argument in the internal `jfa:::print.jfaPrior()`, `jfa:::print.jfaPlanning()`, `jfa:::print.jfaSelection()`, and `jfa:::print.jfaEvaluation()` functions to control rounding in printing.
- Added `description`, `statistics`, `specifics` and `hypotheses` to the output of the `auditPrior()` function.
- Added class `jfaPosterior` with `print()` and `plot()` methods.
- Added `expectedPosterior` of class `jfaPosterior` to the output of the `planning()` function, includes `description`, `statistics` and `hypotheses`.
- Added `posterior` of class `jfaPosterior` to the output of the `evaluation()` function, includes `description`, `statistics` and `hypotheses`.

**Bug fixes**

- Implemented improved calculation of prior parameters in the `auditPrior()` function for `method = median` when `expectedErrors > 0`.

**Major changes**

- Add a warning message to the `sampling()` function that it will be deprecated from 0.5.0 onward. You can use `selection()` instead, since `sampling()` causes namespace issues with other packages.

**Minor changes**

- Changed the class `jfaSampling` to `jfaSelection`. This should not have any consequences. 

# jfa 0.3.1

**Bug fixes**

- Fixed a bug in the `planning()` function that did not allow the user to plan for a monetary sample when their population size was too low.
- Fixed a bug in the `planning()` function that did not allow the user to select a non-integer number of expected errors when there was a prior involved.

**Minor changes**

- Added unit tests that regularly verify results of the `planning()` and `evaluation()` functions against benchmarks.

# jfa 0.3.0

**New features**

- Implemented the argument `counts` in the `evaluation()` function that quantifies how many times each observation should be evaluated due to being selected multiple times in the selection stage.

# jfa 0.2.0

**New features**

- Implemented prior construction methods `none`, `median`, `hypotheses`, `sample`, and `factor` in the `auditPrior()` function. In addition to the already supported `arm` method, these methods allow the auditor to incorporate more sources of audit information into the prior distribution.  
- Implemented `minPrecision` argument in the `planning()` function that allows auditors to calculate a sample size so that the difference between the posterior upper confidence bound and the most likely error is lower than the set minimum precision. Also implemented in the `evaluation()` function as a requirement to approve the population.
- Return the value `mle` from the `evaluation()` function, which quantifies the most likely error. Also return the value of the `precision` from this function.
- Implemented `increase` argument in the `planning()` function that allows the user to increase the sample size with a set amount each step of the iterations.

**Minor changes**

- Implemented more efficient versions of the monetary unit sampling algorithms.
- Changed the x-axis labels in the default plot to theta instead of misstatement.

# jfa 0.1.0

**New features**

- First version of the `jfa` package. The package provides four functions: `auditPrior()`, `planning()`, `sampling()`, and `evaluation()`.