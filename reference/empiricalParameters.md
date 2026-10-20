# Calculate empirical parameters

Returns the empirical parameter estimate for a distribution

## Usage

``` r
empiricalParameters(y, ...)
```

## Arguments

- y:

  output of rtrunc

- ...:

  other arguments passed to methods

## Value

A vector of parameter estimates for the input sample

## Examples

``` r
# Normal distribution
sampNorm <- rtrunc(50, mean = 5, sd = 2)
empiricalParameters(sampNorm)
#>     mean       sd 
#> 5.473322 2.024155 
#> attr(,"class")
#> [1] "parms_normal"

# Poisson distribution
sampPois <- rtrunc(10, lambda = 100, family = "Poisson")
empiricalParameters(sampPois)
#> lambda 
#>  102.6 
#> attr(,"class")
#> [1] "parms_poisson"
```
