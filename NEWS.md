# jfa 0.7.1

**Bug fixes**

- Fixed a bug in `selection()` where the numeric data in the sample was returned as character if the original data was entered as character only (i.e., `selection(data.frame(x = c("hello", "bye", "hi")), size = 2)`).
- `N.units` is now only taken from the prior if the user does not provide it manually to the `planning()` and `evaluation()` functions.

# jfa 0.7.0

**New features**

- Added a new function `model_fairness()` to compute fairness metrics for algorithm auditing on the basis of predictions of an algorithm.

**Minor changes**

- In the `evaluation()` function, the typical stringer bound has been changed from `method = "stringer"` to `method = "stringer.binomial"`. Furthermore, additional methods `stringer.poisson` and `stringer.hypergeometric` are added that apply the stringer bound using the Poisson and hypergeometric distributions, respectively. The `stringer` method will remain for now but redirect to `stringer.binomial`.
- Compatibility with `rstantools 2.3.1`.

# jfa 0.6.7

**New features**

- `digit_test()` function now returns element `estimates` containing observed proportions and their corresponding confidence intervals. The plot output also shows these intervals around the estimates.

# jfa 0.6.6

**Bug fixes**

- Changed the links to the images in the vignettes from absolute links to GitHub to relative links as to avoid problems with GitHub servers being unavailable in the future.

# jfa 0.6.5

**New features**

- Additional possibilities to create, plan and evaluate using nonparametric prior distributions.
- Added functionality for stratified evaluation in the `evaluation()` function.
- Integrated the auditing functionality of the `digitTests` package via the `digit_test()` and `repeated_test()` functions.

**Bug fixes**

- Fixed a bug where the proportion of value in `summary(selection)` was displayed incorrectly.

**Minor changes**

- Improved the error handling and error messages.

# jfa 0.6.4

**Minor changes**

- Resolved CRAN note `Warning: <img> attribute "align" not allowed for HTML5`.
- Improve compatibility with `R 4.2.0`.

# jfa 0.6.3

**Minor changes**

- The value for `x` in `evaluation()` and `expected` in `planning()` is now automatically ceiled (while throwing a warning) for `method = "hypergeometric"`.
- Improved overall documentation.

# jfa 0.6.2

**Major changes**

- Removed the output and functions related to the class `jfaPredictive`. The probabilities for the prior and posterior predictive distributions can be obtained by calling the `predict()` function.

**Minor changes**

- The value for `N.units` in `auditPrior()`, `planning()`, and `evaluation()` is now automatically ceiled for `likelihood`/`method` = `'hypergeometric'`.
- Implemented a warning message when the `likelihood` in the `prior` does not match with the `likelihood`/`method` inputs for `planning()` and `evaluation()`. The likelihood from the prior is leading in these cases and will overrule the other options.

# jfa 0.6.1

**New features**

- Added argument `randomize` to the `selection()` function, which allows the user to randomly shuffle the items in the population before selection. Note that specifying `randomize = TRUE` overrules `order`.

**Bug fixes**

- Fixed a bug where the maximum sample size was not reached (i.e., `planning` stopped at `max` - 1).

**Major changes**

- The `order` argument in `selection()` no longer accepts a logical, but instead takes the column name of the ranking variable in the `data`.

**Minor changes**

- Adjusted an error in the text of the selection vignette.
- Rewritten most of the vignettes.

# jfa 0.6.0

**New features**

- Added argument `alternative` with possible options `less` (default), `two.sided`, and `greater` to the `evaluation()` function that allows control over the type of hypothesis test to perform and the type of confidence / credible interval to calculate.
- Added `predict.jfaPrior()` and `predict.jfaPosterior()` that produce predictions for the data under the prior or posterior distribution.
- Added `method = 'param'` to function `auditPrior()` which takes as input the raw `alpha` and `beta` parameters of the prior distribution.
- Added `method = 'strict'` to function `auditPrior()` which constructs an (improper) prior distribution that yields the same results (with respect to sample sizes and upper limits) as classical procedures.
- Added the modified seed sampling algorithm (`method = 'sieve')` to `selection()`.
- Added a new vignette that describes the sampling methodology implemented in `jfa`.
- objects from `auditPrior()`, `planning()`, and `evaluation()` now contain information about the posterior predictive distribution when `N.units` is specified.

**Major changes**

- From `jfa` 0.5.7 to `jfa` 0.6.0 there has been a major overhaul in the names of function arguments. This is done so that the calls integrate better with general R syntax and the package gets more user-friendly. I apologize for any inconvenience this may cause. The following names have been changed:
  - `median` -> `impartial` (in `auditPrior()`)
  - `sampleK` -> `x` (in `auditPrior()`)
  - `sampleN` -> `n` (in `auditPrior()`)
  - `N` -> `N.units` (in `auditPrior()`)
  - `maxSize` -> `max` (in `planning()`)
  - `increase` -> `by` (in `planning()`)
  - `withReplacement`-> `replace` (in `selection()`)
  - `ordered` -> `order` (in `selection()`)
  - `ascending` -> `decreasing` (in `selection()`)
  - `intervalStartingPoint` -> `start` (in `selection()`)
  - `algorithm` -> `method` (in `selection()`)
  - `expectedErrors` -> `expected` (in `auditPrior()` and `planning()`)
  - `confidence` -> `conf.level` (in `auditPrior()`, `planning()`, and `evaluation()`)
  - `pHmin` -> `p.hmin` (in `auditPrior()`)
  - `minPrecision` -> `min.precision` (in `auditPrior()`, `planning()`, and `evaluation()`)
  - `population` -> `data` (in `selection()`)
  - `kSumstats` -> `x` (in `evaluation()`)
  - `nSumstats` -> `n` (in `evaluation()`)
  - `sample` -> `data` (in `evaluation()`)
  - `bookValues` -> `values` (in `selection()` and `evaluation()`)
  - `auditValues` -> `values.audit` (in `evaluation()`)
  - `counts` -> `times` (in `evaluation()`)
  - `popBookValues` -> `N.units` (in `evaluation()`)
  - `rohrbachDelta` -> `r.delta` (in `evaluation()`)
  - `momentPopType` -> `m.type` (in `evaluation()`)
  - `csA` -> `cs.a` (in `evaluation()`)
  - `csB` -> `cs.b` (in `evaluation()`)
  - `csMu` -> `cs.mu` (in `evaluation()`)
  - `records` -> `items` (in `selection()`)
  - `mus` -> `values` (in `selection()`)
  - `hypotheses` -> `hyp` (in `auditPrior()`)
- `poisson` is now the default likelihood / method for all functions since it is the most conservative.
- `method = 'interval'` is now the default selection method.
- The default prior distributions used when `method = 'default'` or `prior = TRUE` are now set to the `gamma(1, 1)`, `beta(1,1)`, and `beta-binomial(1, 1)` priors.
- The `times` (former `counts`) argument in `evaluation()` must now be indicated as a column name in the `data` instead of a vector.
- `nPrior` and `kPrior` have been removed from the `planning()` and `evaluation()` functions. All prior distributions must now be specified using `prior = TRUE` (noninformative priors) or using a call to `auditPrior()`.
- Removed the `auditBF()` function since its value is available through `evaluation(materiality = x, prior = auditPrior(method = 'impartial', materiality = x))`

**Minor changes**

- It is now allowed for `x` and `n` to have the same value in `evaluation()`.
- The parameters for an impartial beta-binomial prior are now calculated more efficiently in the case of zero expected errors.

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
- Made `method = 'hypotheses'` and `method = 'impartial'` in the `auditPrior()` function available for `likelihood = 'hypergeometric'`.
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

- Implemented improved calculation of prior parameters in the `auditPrior()` function for `method = impartial` when `expectedErrors > 0`.

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

- Implemented prior construction methods `default`, `impartial`, `hypotheses`, `sample`, and `factor` in the `auditPrior()` function. In addition to the already supported `arm` method, these methods allow the auditor to incorporate more sources of audit information into the prior distribution.  
- Implemented `minPrecision` argument in the `planning()` function that allows auditors to calculate a sample size so that the difference between the posterior upper confidence bound and the most likely error is lower than the set minimum precision. Also implemented in the `evaluation()` function as a requirement to approve the population.
- Return the value `mle` from the `evaluation()` function, which quantifies the most likely error. Also return the value of the `precision` from this function.
- Implemented `increase` argument in the `planning()` function that allows the user to increase the sample size with a set amount each step of the iterations.

**Minor changes**

- Implemented more efficient versions of the monetary unit sampling algorithms.
- Changed the x-axis labels in the default plot to theta instead of misstatement.

# jfa 0.1.0

**New features**

- First version of the `jfa` package. The package provides four functions: `auditPrior()`, `planning()`, `sampling()`, and `evaluation()`.