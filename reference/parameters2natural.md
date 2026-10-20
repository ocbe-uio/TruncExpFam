# Convert distribution parameters to natural parameters

Convert distribution parameters to natural parameters

## Usage

``` r
parameters2natural(parms, ...)
```

## Arguments

- parms:

  A vector of parameters in a distribution distribution

- ...:

  other arguments passed to methods

## Value

A vector containing the natural parameters

## See also

[`natural2parameters()`](natural2parameters.md)

## Examples

``` r
# Poisson distribution
samp <- rtrunc(n = 100, lambda = 2, family = "Poisson")
parameters2natural(empiricalParameters(samp))
```
