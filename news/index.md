# Changelog

## TruncExpFam 1.2.2

- Fixed [`rtruncgamma()`](../reference/rtrunc.md) and
  [`rtruncinvgamma()`](../reference/rtrunc.md) with `faster = TRUE`
- Updated package documentation for compatibility with roxygen2 8.

## TruncExpFam 1.2.1

CRAN release: 2025-04-10

- Increased tolerance on unit test (issue
  [\#114](https://github.com/ocbe-uio/TruncExpFam/issues/114))

## TruncExpFam 1.2.0

CRAN release: 2024-09-26

- Implemented [`ptrunc()`](../reference/ptrunc.md) and
  [`qtrunc()`](../reference/qtrunc.md) for all distributions (issue
  [\#54](https://github.com/ocbe-uio/TruncExpFam/issues/54))
- Refactoring (issue
  [\#104](https://github.com/ocbe-uio/TruncExpFam/issues/104),
  [\#112](https://github.com/ocbe-uio/TruncExpFam/issues/112))
- Fixed bugs related to using the Negative Binomial with `mu` instead of
  `prob` (issue
  [\#107](https://github.com/ocbe-uio/TruncExpFam/issues/107))
- Fixed domain validation on Negative Binomial and Inverse Gamma
- Added domain validation to `rtrunc(..., faster = TRUE)` (issue
  [\#109](https://github.com/ocbe-uio/TruncExpFam/issues/109))
- Added `faster` argument to [`rtrunc()`](../reference/rtrunc.md)
  aliases (issue
  [\#110](https://github.com/ocbe-uio/TruncExpFam/issues/110))
- Improved calculation of cumulative densities (issue
  [\#113](https://github.com/ocbe-uio/TruncExpFam/issues/113))

## TruncExpFam 1.1.1

CRAN release: 2024-02-26

- Fixed [`UseMethod()`](https://rdrr.io/r/base/UseMethod.html) no longer
  forwarding local variables from the generic on R-devel (issue
  [\#103](https://github.com/ocbe-uio/TruncExpFam/issues/103))

## TruncExpFam 1.1.0

CRAN release: 2023-11-14

- Added a `NEWS.md` file to track changes to the package.
- Added support for simple numeric-class vectors on
  [`mlEstimationTruncDist()`](../reference/mlEstimationTruncDist.md)
  (issue [\#95](https://github.com/ocbe-uio/TruncExpFam/issues/95))
- Improved linting (issue
  [\#83](https://github.com/ocbe-uio/TruncExpFam/issues/83))
- Improved unit test coverage (issues
  [\#66](https://github.com/ocbe-uio/TruncExpFam/issues/66),
  [\#82](https://github.com/ocbe-uio/TruncExpFam/issues/82))
- Improved output format (issues
  [\#89](https://github.com/ocbe-uio/TruncExpFam/issues/89),
  [\#94](https://github.com/ocbe-uio/TruncExpFam/issues/94))
- Improved documentation (issue
  [\#86](https://github.com/ocbe-uio/TruncExpFam/issues/86))
- Improved ML estimation (issues
  [\#84](https://github.com/ocbe-uio/TruncExpFam/issues/84),
  [\#85](https://github.com/ocbe-uio/TruncExpFam/issues/85),
  [\#90](https://github.com/ocbe-uio/TruncExpFam/issues/90),
  [\#92](https://github.com/ocbe-uio/TruncExpFam/issues/92))
- Improved parameter parsing (issue
  [\#74](https://github.com/ocbe-uio/TruncExpFam/issues/74))
- Simplified codebase (issues
  [\#91](https://github.com/ocbe-uio/TruncExpFam/issues/91),
  [\#77](https://github.com/ocbe-uio/TruncExpFam/issues/77),
  [\#78](https://github.com/ocbe-uio/TruncExpFam/issues/78))
- Added a vignette (issue
  [\#53](https://github.com/ocbe-uio/TruncExpFam/issues/53))
- Enabled usage of
  [`parameters2natural()`](../reference/parameters2natural.md) and
  [`natural2parameters()`](../reference/natural2parameters.md) with
  numeric vectors (issue
  [\#88](https://github.com/ocbe-uio/TruncExpFam/issues/88))
- Faster calculation of [`rtrunc()`](../reference/rtrunc.md) for
  continuous distributions (issue
  [\#78](https://github.com/ocbe-uio/TruncExpFam/issues/78))
- Faster calculation of [`rtrunc()`](../reference/rtrunc.md) for
  discrete distributions (issue
  [\#77](https://github.com/ocbe-uio/TruncExpFam/issues/77))
- Created package page on <https://ocbe-uio.github.io/TruncExpFam/>
  (issues [\#100](https://github.com/ocbe-uio/TruncExpFam/issues/100),
  [\#101](https://github.com/ocbe-uio/TruncExpFam/issues/101))
- Reclassified `eta` and `parms` arguments as `parms_*` instead of
  `trunc_*` (issue
  [\#97](https://github.com/ocbe-uio/TruncExpFam/issues/97))

## TruncExpFam 1.0.1

CRAN release: 2023-08-22

- Hotfix for package documentation
  (<https://github.com/r-lib/roxygen2/issues/1491>)
- Improved argument consistency between generic and methods

## TruncExpFam 1.0.0

CRAN release: 2022-02-24

- CRAN debut
