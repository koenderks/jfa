## This is a resubmission for version 0.2.0
This is jfa version 0.4.0. In this version I have:

* Added more input and output options to the auditPrior, planning, and evaluation functions.
* Created a new function selection() that replaces the sampling() function. Now gives a warning that sampling() will be deprecated from 0.5.0 onwards.
* Added digits argument in the print functions to control rounding and added more output to functions.

## Test environments
* OS X install (on travis-ci), R release
* Linux install (on travis-ci), R release

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 

## Downstream dependencies
There are currently no downstream dependencies for this package.