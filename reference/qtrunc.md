# Quantile Function

Calculates quantile for a given truncated distribution and probability.

## Usage

``` r
qtrunc(p, family, ..., lower.tail = TRUE, log.p = FALSE)

qtruncbeta(
  p,
  shape1,
  shape2,
  a = 0,
  b = 1,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

qtruncbinom(
  p,
  size,
  prob,
  a = 0,
  b = size,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

qtruncchisq(p, df, a = 0, b = Inf, ..., lower.tail = TRUE, log.p = FALSE)

qtrunccontbern(p, lambda, a = 0, b = 1, ..., lower.tail = TRUE, log.p = FALSE)

qtruncexp(p, rate = 1, a = 0, b = Inf, ..., lower.tail = TRUE, log.p = FALSE)

qtruncgamma(
  p,
  shape,
  rate = 1,
  scale = 1/rate,
  a = 0,
  b = Inf,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

qtruncinvgamma(
  p,
  shape,
  rate = 1,
  scale = 1/rate,
  a = 0,
  b = Inf,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

qtruncinvgauss(p, m, s, a = 0, b = Inf, ..., lower.tail = TRUE, log.p = FALSE)

qtrunclnorm(
  p,
  meanlog = 0,
  sdlog = 1,
  a = 0,
  b = Inf,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

qtruncnbinom(
  p,
  size,
  prob,
  mu,
  a = 0,
  b = Inf,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

qtruncnorm(
  p,
  mean = 0,
  sd = 1,
  a = -Inf,
  b = Inf,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

qtruncpois(p, lambda, a = 0, b = Inf, ..., lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- p:

  vector of quantiles

- family:

  distribution family to use

- ...:

  *named* distribution parameters and/or truncation limits (`a`, `b`)

- lower.tail:

  logical; if `TRUE`, probabilities are \\P(X \<= x)\\ otherwise, \\P(X
  \> x)\\

- log.p:

  logical; if `TRUE`, probabilities p are given as `log(p)`

- shape1:

  positive shape parameter alpha

- shape2:

  positive shape parameter beta

- a:

  point of left truncation. For discrete distributions, `a` will be
  included in the support of the truncated distribution.

- b:

  point of right truncation

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

- mu:

  alternative parametrization via mean

- mean:

  mean of parent distribution

- sd:

  standard deviation is parent distribution

## Value

The quantile of `p`.

## Examples

``` r
qtrunc(0.75)
#> [1] 0.6744898
qtrunc(.2, family = "gaussian", mean = 5, sd = 10, b = 7)
#> [1] -6.959812
qnorm(.2, mean = 5, sd = 10) # for comparison
#> [1] -3.416212
```
