## Version 0.2.0
This is jfa version 0.2.0. In this version I have:

* Implemented prior construction methods `none`, `median`, `hypotheses`, `sample`, and `factor` in the `auditPrior()` function.  
* Implemented `minPrecision` argument in the `planning()` function and the `Evaluation()` function.
* Return the value `mle` and the value of the `precision` from the `Evaluation()` function.
* Implemented `increase` argument in the `planning()` function.
* Implemented more efficient versions of certain algorithms in the `sampling()` function.
* Changed the x-axis labels in the default plot to theta instead of misstatement.

## Test environments
* OS X install (on travis-ci), R release
* Linux install (on travis-ci), R release

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 

## Downstream dependencies
There are currently no downstream dependencies for this package.