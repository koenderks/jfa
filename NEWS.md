# jfa 0.4.0

- Added Expected Bayes factors, expected upper bound, and expected precision to the output of the `planning()` function.
- Added Bayes factors to the output of the `evaluation()` function.
- Implemented calculation of prior parameters in the `auditPrior()` function for `method = median` when `expectedErrors > 0`.
- Added `digits` argument in the internal `jfa:::print.jfaPrior()`, `jfa:::print.jfaPlanning()`, `jfa:::print.jfaSampling()`, and `jfa:::print.jfaEvaluation()` functions to control rounding.

# jfa 0.3.1

- Added unit tests that regularly check results of the `planning()` and `evaluation()` functions.
- Fixed two bugs in the `planning()` function. The first bug did not allow the user to plan for a monetary sample when their population size was too low. The second bug did not allow the user to select a non-integer number of expected errors when there was a prior involved.

# jfa 0.3.0

- Implemented the argument `counts` in the `evaluation()` function that quantifies how many times each observation should be evaluated due to being selected multiple times in the selection stage.

# jfa 0.2.0

- Implemented prior construction methods `none`, `median`, `hypotheses`, `sample`, and `factor` in the `auditPrior()` function. In addition to the already supported `arm` method, these methods allow the auditor to incorporate more sources of audit information into the prior distribution.  

- Implemented `minPrecision` argument in the `planning()` function that allows auditors to calculate a sample size so that the difference between the posterior upper confidence bound and the most likely error is lower than the set minimum precision. Also implemented in the `evaluation()` function as a requirement to approve the population.

- Return the value `mle` from the `evaluation()` function, which quantifies the most likely error. Also return the value of the `precision` from this function.

- Implemented `increase` argument in the `planning()` function that allows the user to increase the sample size with a set amount each step of the iterations.

- Implemented more efficient versions of the monetary unit sampling algorithms.

- Changed the x-axis labels in the default plot to theta instead of misstatement.

# jfa 0.1.0

- First version of the `jfa` package. The package provides four functions: `auditPrior()`, `planning()`, `sampling()`, and `evaluation()`.