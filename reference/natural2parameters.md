# Convert natural parameters to distribution parameters

Convert natural parameters to distribution parameters

## Usage

``` r
natural2parameters(eta, ...)
```

## Arguments

- eta:

  vector of natural parameters

- ...:

  other arguments passed to methods

## Value

A vector of the original distribution parameters

## See also

[`parameters2natural()`](parameters2natural.md)

## Examples

``` r
samp <- rtrunc(n = 100, lambda = 2, family = "Poisson")
lambda_hat <- empiricalParameters(samp)
eta_hat <- parameters2natural(lambda_hat)
natural2parameters(eta_hat)  # yields back lambda
#> lambda 
#>   2.15 
#> attr(,"class")
#> [1] "parms_poisson"
```
