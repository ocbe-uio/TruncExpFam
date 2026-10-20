# Sampling and ML estimation

## Using TruncExpFam

We recommend installing the stable, peer-reviewed version of
TruncExpFam, available on CRAN:

``` r

install.packages("TruncExpFam")
```

After successful installation, the package can be loaded with

``` r

library(TruncExpFam)
#> Welcome to TruncExpFam 1.2.2.
#> Please read the documentation on ?TruncExpFam to learn more about the package.
```

## Sampling from a truncated distribution

TruncExpFam comes equipped with functions to generate random samples
from no less than 12 different probability distributions from the
truncated exponential family. You can read more about them by running
[`?rtrunc`](../reference/rtrunc.md) on your R console.

As an example, we will sample 100 values from a chi-square distribution
with 14 degrees of freedom:

``` r

x <- rtrunc(100, family = "chisq", df = 14)
```

### Different ways to do the same thing

By default, however, [`rtrunc()`](../reference/rtrunc.md) doesn’t
generate a truncated distribution. As a matter of fact, the code above
will generate the exact same sample as if were drawn from
[`stats::rchisq()`](https://rdrr.io/r/stats/Chisquare.html), watch:

``` r

set.seed(3067)
x2 <- rtrunc(20, "chisq", df = 14)

set.seed(3067)
x3 <- rchisq(20, 14)

identical(x2, x3)
#> [1] FALSE
```

Oh, wait… Those objects are supposed to be identical! What happened?
Let’s investigate:

``` r

x2
#>  [1] 16.531982 10.021074 12.480308 16.165519 11.083118 32.684427 16.661472
#>  [8] 18.085124 10.921481 11.150269 10.673091 12.012880  7.986689  7.500130
#> [15] 10.951995  6.725427 10.789780  5.616512 20.081876  8.138363
x3
#>  [1] 16.531982 10.021074 12.480308 16.165519 11.083118 32.684427 16.661472
#>  [8] 18.085124 10.921481 11.150269 10.673091 12.012880  7.986689  7.500130
#> [15] 10.951995  6.725427 10.789780  5.616512 20.081876  8.138363
str(x2)
#>  'trunc_chisq' num [1:20] 16.5 10 12.5 16.2 11.1 ...
#>  - attr(*, "parameters")=List of 1
#>   ..$ df: num 14
#>  - attr(*, "truncation_limits")=List of 2
#>   ..$ a: num 0
#>   ..$ b: num Inf
#>  - attr(*, "continuous")= logi TRUE
str(x3)
#>  num [1:20] 16.5 10 12.5 16.2 11.1 ...
class(x2)
#> [1] "trunc_chisq"
class(x3)
#> [1] "numeric"
```

OK, so you can tell that the generated numbers are the same, but `x2`
and `x3` are not literally the same objects because the former has a
different class. These `trunc_*` classes are actually very special,
because they contain some extra information about the distribution that
a simple vector does not. One can access such information using
`print(x2, details = TRUE)`:

``` r

print(x2, details = TRUE)
#>  [1] 16.531982 10.021074 12.480308 16.165519 11.083118 32.684427 16.661472
#>  [8] 18.085124 10.921481 11.150269 10.673091 12.012880  7.986689  7.500130
#> [15] 10.951995  6.725427 10.789780  5.616512 20.081876  8.138363
#> attr(,"class")
#> [1] "trunc_chisq"
#> attr(,"parameters")
#> attr(,"parameters")$df
#> [1] 14
#> 
#> attr(,"truncation_limits")
#> attr(,"truncation_limits")$a
#> [1] 0
#> 
#> attr(,"truncation_limits")$b
#> [1] Inf
#> 
#> attr(,"continuous")
#> [1] TRUE
```

Just to be sure that the sample itself matches:

``` r

identical(as.vector(x2), x3)
#> [1] TRUE
```

Speaking of alternative ways to generate the same sample, for the sake
of convenience and of users familiar with the sampling functions from
the stats package, the wrapper function
[`rtruncchisq()`](../reference/rtrunc.md) is also available. The
results, as you can see below, are identical:

``` r

set.seed(2912)
x4 <- rtrunc(1e4, "chisq", df = 14)

set.seed(2912)
x5 <- rtruncchisq(1e4, df = 14)

identical(x4, x5)
#> [1] TRUE
```

### Actually sampling from a truncated distribution

So far, all samples generated are actually not truncated. This is
because, by default, the truncation limits `a` and `b` are set to the
limits of the distribution support, which are 0 and Inf for the
chi-squared distribution.

Let us use a simpler distribution for this second example by sampling
from Poisson(10):

``` r

y1 <- rtruncpois(1e4, 10)
summary(y1)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    0.00    8.00   10.00   10.01   12.00   26.00
var(y1)
#> [1] 10.04213
```

As expected, the values are all larger than 0 and the mean and variance
are 10. If we wanted to generate instead from a Poisson(10) truncated
at, say, 8 and 20, we would run:

``` r

y2 <- rtruncpois(1e4, 10, a = 9, b = 20)
summary(y2)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    9.00   10.00   11.00   11.63   13.00   20.00
var(y2)
#> [1] 5.159541
```

Notice how, even with a large sample, the observed mean and variance are
still quite far from 10.

## Recovering the original parameters

One reliable method of estimating the original lambda used to generate
`y2` is by running the
[`mlEstimationTruncDist()`](../reference/mlEstimationTruncDist.md)
function:

``` r

lambda <- mlEstimationTruncDist(y2, print.iter = TRUE)
#> Estimating parameters for the poisson distribution
#>  it   delta.L2    parameter(s)
#>  1    0.0007325   11.626 
#>  2    0.0005043   11.315 
#>  3    0.0003465   11.064 
#>  4    0.0002378   10.86 
#>  5    0.0001631   10.694 
#>  6    0.0001119   10.558 
#>  7    7.674e-05   10.447 
#>  8    5.264e-05   10.356 
#>  9    3.611e-05   10.281 
#> 10    2.478e-05   10.219 
#> 11      1.7e-05   10.169 
#> 12    1.167e-05   10.127 
#> 13    8.007e-06   10.092
lambda
#>   lambda 
#> 10.06366
```

More information about that function and how you can tweak it is
available on
[`?mlEstimationTruncDist`](../reference/mlEstimationTruncDist.md).
