# jfa version 0.6.6

This is a resubmission for version 0.6.6. In this version I have:

* Attempted to fix the `clang-UBSAN` error `caster.h:30:25: runtime error: -468 is outside the range of representable values of type 'unsigned int'` by preventing negative values for the `seed` argument in the call to `rstan::sampling`. There is an open issue on the `rstan` GitHub issue tracker (titled `seed = NA results in a clang-UBSAN error on CRAN`), which discusses the same `clang-UBSAN` error as a result of an `NA` input for the `seed`. Hence, I suspect the root of the problem to be an invalid (negative) value for `rstan`'s seed. I have fixed this in this submission of `jfa`. Would you please be so kind as to test with this fix? If the problem is resolved, I can give the `rstan` team more information regarding negative seed inputs.

## Test environments

* Windows install (on GitHub actions), R release
* OS X install (on GitHub actions), R release
* Linux install (on GitHub actions), R release

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTES:

* checking installed package size ... NOTE. installed size is 5.7Mb. sub-directories of 1Mb or more: doc 2.3Mb; lib: 2.7Mb
* GNU make is a SystemRequirements.

## Downstream dependencies
There are currently no downstream dependencies for this package.
