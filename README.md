<img src="logo.png" width=25%>

# TruncExpFam

This is an R package to handle truncated members from the exponential family.

# Installation

TruncExpFam is currently under development. To install the package, first make sure you have either the "remotes" or the "devtools" package installed in your local R installation, then issue the following command:

```r
remotes::install_github("ocbe-uio/TruncExpFam")
```

Finally, load the package with `library(TruncExpFam)`. A list of available functions can be printed with `ls("package:TruncExpFam")`.

Further details on installing TruncExpFam can be found on the [Wiki](https://github.com/ocbe-uio/TruncExpFam/wiki/Installing-TruncExpFam).

# Usage

For more information about the package (e.g. suppored distributions), run `?TruncExpFam` after loading the package in your R session.

Are you familiar with the stats package and its `r*` and `d*` functions such as `rnorm()` and `dpois()`? If so, you will feel right at home with `TruncExpFam`, which uses the `rtrunc()` function to generate random numbers and the `dtrunc()` function to generate probability densities.

# Contributing

TruncExpFam is open-source software licensed by the GPL. All contributions are welcome! Please use the [issues page](https://github.com/ocbe-uio/TruncExpFam/issues) to submit any bugs you find or see what other issues have been submitted.

To contribute with code, we recommend reading [this Wiki page](https://github.com/ocbe-uio/TruncExpFam/wiki/Contributing-to-the-project) on the subject.

# Badges

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/github/last-commit/ocbe-uio/TruncExpFam.svg)](https://github.com/ocbe-uio/TruncExpFam/commits/develop)
[![](https://img.shields.io/github/languages/code-size/ocbe-uio/TruncExpFam.svg)](https://github.com/ocbe-uio/TruncExpFam)
[![R build status](https://github.com/ocbe-uio/TruncExpFam/workflows/R-CMD-check/badge.svg)](https://github.com/ocbe-uio/TruncExpFam/actions)
[![codecov](https://codecov.io/gh/ocbe-uio/TruncExpFam/branch/develop/graph/badge.svg?token=78YFRZKJO6)](https://codecov.io/gh/ocbe-uio/TruncExpFam)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![CodeFactor](https://www.codefactor.io/repository/github/ocbe-uio/TruncExpFam/badge)](https://www.codefactor.io/repository/github/ocbe-uio/TruncExpFam)
