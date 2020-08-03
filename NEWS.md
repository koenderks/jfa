# jfa 0.2.0

- Implemented prior construction methods `none`, `median`, `hypotheses`, `sample`, and `factor` in the `auditPrior()` function. In addition to the already supported `arm` method, these methods allow the auditor to icorporate more sources of audit information into the prior distribution.  

- Implemented `minPrecision` argument in the `planning()` function that allows auditors to calculate a sample size so that the difference between the posterior upper confidence bound and the most likely error is lower than the set minimum precision.

- Implemented `increase` argument in the `planning()` function that allows the user to increase the sample size with a set amount each step of the iterations.

- Implemented a more efficient monetary unit sampling algorithm.

- Changed the x-axis labels in the default plot to theta instead of misstatement.

# jfa 0.1.0

- First version of the `jfa` package. The package provides four functions: `auditPrior()`, `planning()`, `sampling()`, and `evaluation()`.