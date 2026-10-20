# ML Estimation of Distribution Parameters

ML-estimation of the parameters of the distribution of the specified
family, truncated at y.min and y.max

## Usage

``` r
mlEstimationTruncDist(
  y,
  y.min = attr(y, "truncation_limits")$a,
  y.max = attr(y, "truncation_limits")$b,
  tol = 1e-05,
  max.it = 100,
  delta = 0.33,
  print.iter = 0,
  ny = 100,
  family = NULL,
  ...
)
```

## Arguments

- y:

  Sequence spanning the domain of the truncated distribution

- y.min:

  Lower bound for `y`

- y.max:

  Upper bound for `y`

- tol:

  Error tolerance for parameter estimation

- max.it:

  Maximum number of iterations

- delta:

  Indirectly, the difference between consecutive iterations to compare
  with the error tolerance

- print.iter:

  Determines the frequency of printing (i.e., prints every `print.iter`
  iterations)

- ny:

  size of intermediate `y` range sequence. Higher values yield better
  estimations but slower iterations

- family:

  distribution family to use

- ...:

  other parameters passed to subfunctions

## Value

A vector of class `trunc_*` containing the maximum-likelihood estimation
of the underlying distribution \* parameters.

## Details

If `print.iter = TRUE`, the function prints the iteration, the sum of
squares of `delta.eta.j` (`delta.L2`), and the current parameter
estimates. The `delta` argument of this function is a factor in the
calculation of `delta.eta.j`, which in turn is a factor in the
calculation of `delta.L2`.

## References

Inspired by Salvador: Pueyo: "Algorithm for the maximum likelihood
estimation of the parameters of the truncated normal and lognormal
distributions"

## Author

René Holst

## Examples

``` r
sample_size <- 1000
# Normal
sample.norm <- rtrunc(n = sample_size, mean = 2, sd = 1.5, a = -1)
mlEstimationTruncDist(
  sample.norm,
  y.min = -1, max.it = 500, delta = 0.33,
  print.iter = TRUE
)
#> Estimating parameters for the normal distribution
#>  it   delta.L2    parameter(s)
#>  1     0.001548   2.053 1.419 
#>  2     0.000829   2.034 1.44 
#>  3    0.0004685   2.02 1.456 
#>  4     0.000275   2.008 1.469 
#>  5    0.0001661   2 1.479 
#>  6    0.0001025   1.993 1.486 
#>  7    6.426e-05   1.987 1.493 
#>  8    4.082e-05   1.983 1.498 
#>  9    2.618e-05   1.979 1.502 
#> 10    1.693e-05   1.976 1.505 
#> 11    1.102e-05   1.974 1.507 
#> 12    7.206e-06   1.972 1.509 
#>     mean       sd 
#> 1.970295 1.511188 

# Log-Normal
sample.lognorm <- rtrunc(
  n = sample_size, family = "lognormal", meanlog = 2.5, sdlog = 0.5, a = 7
)
ml_lognormal <- mlEstimationTruncDist(
  sample.lognorm,
  y.min = 7, max.it = 500, tol = 1e-10, delta = 0.3,
  print.iter = FALSE
)
ml_lognormal
#>   meanlog     sdlog 
#> 2.5022664 0.4840911 

# Poisson
sample.pois <- rtrunc(
 n = sample_size, lambda = 10, a = 4, family = "Poisson"
)
mlEstimationTruncDist(
  sample.pois,
  y.min = 4, max.it = 500, delta = 0.33,
  print.iter = 5
)
#> Estimating parameters for the poisson distribution
#>  it   delta.L2    parameter(s)
#>   lambda 
#> 10.02048 

# Gamma
sample.gamma <- rtrunc(
 n = sample_size, shape = 6, rate = 2, a = 2, family = "Gamma"
)
mlEstimationTruncDist(
  sample.gamma,
  y.min = 2, max.it = 1500, delta = 0.3,
  print.iter = 10
)
#> Estimating parameters for the gamma distribution
#>  it   delta.L2    parameter(s)
#>  10     0.01659   8.478 2.632 
#>  20    0.002956   7.621 2.415 
#>  30   0.0007513   7.23 2.314 
#>  40   0.0002285   7.025 2.262 
#>  50   7.641e-05   6.91 2.232 
#>  60   2.694e-05   6.842 2.214 
#>  70   9.796e-06   6.802 2.204 
#>    shape     rate 
#> 6.798898 2.203159 

# Negative binomial
sample.nbinom <- rtruncnbinom(
 sample_size, size = 50, prob = .3, a = 100, b = 120
)
mlEstimationTruncDist(sample.nbinom, r=10)
#> Warning: Maximum number of iterations reached. Convergence is not guaranteed. You might want to run again with a higher value for max.it
#>     mean 
#> 63.86604 
```
