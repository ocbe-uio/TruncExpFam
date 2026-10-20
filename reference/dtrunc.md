# Probability Density Function

Calculates the PDF for a given truncated distribution

## Usage

``` r
dtruncbeta(y, shape1, shape2, eta, a = 0, b = 1, ...)

dtruncbinom(y, size, prob, eta, a = 0, b = attr(y, "parameters")$size, ...)

dtruncchisq(y, df, eta, a = 0, b = Inf, ...)

dtrunccontbern(y, lambda, eta, a = 0, b = 1, ...)

dtrunccontbern(y, lambda, eta, a = 0, b = 1, ...)

dtrunc(y, ...)

dtruncexp(y, rate = 1, eta, a = 0, b = Inf, ...)

dtruncgamma(y, shape, rate = 1, scale = 1/rate, eta, a = 0, b = Inf, ...)

dtruncinvgamma(y, shape, rate = 1, scale = 1/rate, eta, a = 0, b = Inf, ...)

dtruncinvgauss(y, m, s, eta, a = 0, b = Inf, ...)

dtrunclnorm(y, meanlog = 0, sdlog = 1, eta, a = 0, b = Inf, ...)

# S3 method for class 'trunc_nbinom'
dtrunc(y, size, prob, eta, a = 0, b = Inf, ...)

dtruncnbinom(y, size, prob, eta, a = 0, b = Inf, ...)

dtruncnbinom(y, size, prob, eta, a = 0, b = Inf, ...)

dtruncnorm(y, mean = 0, sd = 1, eta, a = -Inf, b = Inf, ...)

dtruncpois(y, lambda, eta, a = 0, b = Inf, ...)
```

## Arguments

- y:

  output from rtrunc or any valid numeric value(s).

- shape1:

  positive shape parameter alpha

- shape2:

  positive shape parameter beta

- eta:

  vector of natural parameters

- a:

  point of left truncation. For discrete distributions, `a` will be
  included in the support of the truncated distribution.

- b:

  point of right truncation

- ...:

  size

- size:

  target for number of successful trials, or dispersion parameter (the
  shape parameter of the gamma mixing distribution). Must be strictly
  positive, need not be integer.

- prob:

  probability of success on each trial

- df:

  degrees of freedom for "parent" distribution

- lambda:

  mean and var of "parent" distribution

- rate:

  inverse gamma rate parameter

- shape:

  inverse gamma shape parameter

- scale:

  inverse gamma scale parameter

- m:

  vector of means

- s:

  vector of dispersion parameters

- meanlog:

  mean of untruncated distribution

- sdlog:

  standard deviation of untruncated distribution

- mean:

  mean of parent distribution

- sd:

  standard deviation is parent distribution

## Value

The density of `y` for the given values of the `eta` parameter.

## Note

Either the common or the natural parameters must be provided.

## Examples

``` r
# Using the output of rtrunc
y <- rtrunc(50, mean = 5, sd = 2)
dtrunc(y, eta = c(0, -1))
#>  [1] 4.462786e-03 3.657347e-14 5.553769e-01 8.757914e-12 6.671209e-18
#>  [6] 4.246301e-24 8.962777e-02 8.630589e-10 8.157554e-10 1.624695e-09
#> [11] 1.481991e-07 5.540320e-18 3.544774e-37 2.751339e-02 9.705737e-17
#> [16] 1.113160e-01 9.012188e-08 2.218966e-11 4.628694e-17 2.410721e-05
#> [21] 2.798921e-16 3.255845e-15 1.856932e-03 3.469359e-19 1.973140e-34
#> [26] 5.296207e-11 3.171696e-05 1.076871e-11 7.726105e-06 1.140410e-02
#> [31] 1.777203e-21 2.027647e-13 4.723673e-14 1.633241e-30 7.926959e-13
#> [36] 1.063543e-10 1.402467e-01 1.527683e-09 2.792192e-09 4.412097e-23
#> [41] 1.893335e-12 5.442740e-07 2.107249e-11 9.301532e-10 4.863101e-16
#> [46] 5.910019e-49 3.062917e-12 1.978639e-17 6.968944e-13 1.413302e-01

# Directly-inputting values
dtruncnorm(y = c(5, 0, -10), eta = c(0, -0.05))
#> [1] 0.0361444785 0.1261566261 0.0008500367
```
