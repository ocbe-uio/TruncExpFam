# Probability distribution class

An R object describing the properties of a probability distribution.

## Value

An RC class containing statistical properties of that distribution,
namely its name, parameter names and values and natural parameter names
and values.

## Author

Waldir Leoncio

## Examples

``` r
probdist(shape = 2, scale = .25, family = "gamma")
#> Family:             Gamma
#> Parameters:         shape = 2    scale = 0.25 
#> Natural parameters: eta1  = 1    eta2  = -4   
probdist(mean = 2, sd = 10, family = "normal")
#> Family:             Normal
#> Parameters:         mean = 2      sd   = 10     
#> Natural parameters: eta1 = 0.02   eta2 = -0.005 
probdist(eta1 = 2, eta2 = -1, family = "normal")
#> Family:             Normal
#> Parameters:         mean = 1                 sd   = 0.7071            
#> Natural parameters: eta1 = 2                 eta2 = -1                
```
