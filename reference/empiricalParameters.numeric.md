# Extract parameters

Extract parameters

## Usage

``` r
# S3 method for class 'numeric'
empiricalParameters(y, family = "gaussian", natural = FALSE, ...)
```

## Arguments

- y:

  Numeric vector containing observations from a random variable

- family:

  Distribution family to assume for `y`

- natural:

  Should output be in terms of the natural parameter eta?

- ...:

  arguments passed to [`empiricalParameters()`](empiricalParameters.md)

## Examples

``` r
# Some random data
x <- c(
  4, 3, 6, 3, 3, 3, 3, 4, 3, 2, 3, 0, 4, 2, 0, 1, 4, 3, 0, 0, 2, 3, 0, 3, 7,
  2, 1, 1, 2, 3, 2, 3, 3, 3, 2, 2, 2, 0, 2, 0, 2, 1, 0, 2, 3, 1, 0, 4, 2, 2,
  0, 1, 1, 1, 2, 2, 3, 1, 3, 1, 1, 0, 3, 3, 2, 0, 2, 2, 3, 0, 2, 1, 0, 0, 1,
  0, 2, 4, 2, 3, 3, 0, 1, 0, 5, 2, 4, 2, 7, 4, 4, 1, 2, 4, 3, 2, 4, 3, 1, 3
)

# Extracting parameters under different distribution assumptions
empiricalParameters(x, family = "normal")
#>    mean      sd 
#> 2.12000 1.52607 
#> attr(,"class")
#> [1] "parms_normal"
empiricalParameters(x, family = "normal", natural = TRUE)
#>       eta1       eta2 
#>  0.9103053 -0.2146947 
#> attr(,"class")
#> [1] "parms_normal"
empiricalParameters(x, family = "binomial", nsize = max(x))
#>      size      prob 
#> 7.0000000 0.3028571 
#> attr(,"class")
#> [1] "parms_binomial"
empiricalParameters(x, family = "poisson", natural = FALSE)
#> lambda 
#>   2.12 
#> attr(,"class")
#> [1] "parms_poisson"
empiricalParameters(x, family = "poisson", natural = TRUE)
#>       eta 
#> 0.7514161 
#> attr(,"class")
#> [1] "parms_poisson"
```
