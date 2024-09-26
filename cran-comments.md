Dear CRAN Team,
this is a resubmission of package 'TruncExpFam'. I have added the following changes:

* Implemented `ptrunc()` and `qtrunc()` for all distributions (issue #54)
* Refactoring (issue #104, #112)
* Fixed bugs related to using the Negative Binomial with `mu` instead of `prob` (issue #107)
* Fixed domain validation on Negative Binomial and Inverse Gamma
* Added domain validation to `rtrunc(..., faster = TRUE)` (issue #109)
* Added `faster` argument to `rtrunc()` aliases (issue #110)
* Improved calculation of cumulative densities (issue #113)

Please upload to CRAN.
Best, Waldir

# Package TruncExpFam 1.2.0

Reporting is done by packager version 1.15.2


## Test environments
- R version 4.3.3 (2024-02-29)
   Platform: x86_64-pc-linux-gnu (64-bit)
   Running under: Ubuntu 24.04.1 LTS
   ERROR: No check log found!
- win-builder (devel)

## Local test results

## Local meta results
