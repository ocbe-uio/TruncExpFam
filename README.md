<img src="logo.png" width=25%>

What is this?
=============

This is an R package to handle truncated members from the exponential
family.

Installation
============

Stable version
--------------

TruncExpFam is available on CRAN and can be installed by running the
following in an interactive R session:

```r
install.packages("TruncExpFam")
```

Development version
-------------------

The development version of the package contains features and bug fixes
that are yet to be published. It is, however, much less stable than the
CRAN version. You can install the development version of TruncExpFam by
running the following command in R (requires the `remotes` package to be
installed beforehand):

```r
remotes::install_github("ocbe-uio/TruncExpFam")
```

If you want to browse the vignette, add `build_vignettes = TRUE` to your `install_github()` command.


Further details on installing TruncExpFam can be found on the
[Wiki](https://github.com/ocbe-uio/TruncExpFam/wiki/Installing-TruncExpFam).

Usage
=====

Once installed, `TruncExpFam` can be loaded with `library(TruncExpFam)`. A list of
available functions can be printed with `ls("package:TruncExpFam")`.

For more information about the package (e.g. suppored distributions),
run `?TruncExpFam` after loading the package in your R session.

Are you familiar with the stats package and its `r*` and `d*` functions
such as `rnorm()` and `dpois()`? If so, you will feel right at home with
`TruncExpFam`, which uses the `rtrunc()` function to generate random
numbers and the `dtrunc()` function to generate probability densities.

For a more detailed explanation on how to use this package’s features,
check out its vignette:

```r
browseVignettes("TruncExpFam")
```

Contributing
============

TruncExpFam is open-source software licensed by the GPL. All
contributions are welcome! Please use the [issues
page](https://github.com/ocbe-uio/TruncExpFam/issues) to submit any bugs
you find or see what other issues have been submitted.

To contribute with code, we recommend reading [this Wiki
page](https://github.com/ocbe-uio/TruncExpFam/wiki/Contributing-to-the-project)
on the subject.

Citing
======

If you present work that uses this package, please remember to cite it. To cite TruncExpFam in publications, use the output of `citation("TruncExpFam")` on your R session.

Badges
======

Stable version
--------------

[![CRAN
version](https://www.r-pkg.org/badges/version/TruncExpFam?color=green)](https://cran.r-project.org/package=TruncExpFam)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/last-month/TruncExpFam?color=green)](https://cran.r-project.org/package=TruncExpFam)
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![DOI](https://zenodo.org/badge/326590808.svg)](https://zenodo.org/badge/latestdoi/326590808)

Development version
-------------------

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Last
commit](https://img.shields.io/github/last-commit/ocbe-uio/TruncExpFam.svg)](https://github.com/ocbe-uio/TruncExpFam/commits/develop)
[![Code
size](https://img.shields.io/github/languages/code-size/ocbe-uio/TruncExpFam.svg)](https://github.com/ocbe-uio/TruncExpFam)
[![R build
status](https://github.com/ocbe-uio/TruncExpFam/workflows/R-CMD-check/badge.svg)](https://github.com/ocbe-uio/TruncExpFam/actions)
[![codecov](https://codecov.io/gh/ocbe-uio/TruncExpFam/branch/develop/graph/badge.svg?token=78YFRZKJO6)](https://codecov.io/gh/ocbe-uio/TruncExpFam)
[![CodeFactor](https://www.codefactor.io/repository/github/ocbe-uio/TruncExpFam/badge)](https://www.codefactor.io/repository/github/ocbe-uio/TruncExpFam)
