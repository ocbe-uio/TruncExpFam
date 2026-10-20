# Cumulative Distribution Function

Calculates the cumulative probability for a given truncated distribution

## Usage

``` r
ptrunc(q, family, ..., lower.tail = TRUE, log.p = FALSE)

ptruncnorm(
  q,
  mean = 0,
  sd = 1,
  a = -Inf,
  b = Inf,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

ptruncbeta(
  q,
  shape1,
  shape2,
  a = 0,
  b = 1,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

ptruncbinom(
  q,
  size,
  prob,
  a = 0,
  b = size,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

ptruncpois(q, lambda, a = 0, b = Inf, ..., lower.tail = TRUE, log.p = FALSE)

ptruncchisq(q, df, a = 0, b = Inf, ..., lower.tail = TRUE, log.p = FALSE)

ptrunccontbern(q, lambda, a = 0, b = 1, ...)

ptruncexp(q, rate = 1, a = 0, b = Inf, ..., lower.tail = TRUE, log.p = FALSE)

ptruncgamma(
  q,
  shape,
  rate = 1,
  scale = 1/rate,
  a = 0,
  b = Inf,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

ptruncinvgamma(
  q,
  shape,
  rate = 1,
  scale = 1/rate,
  a = 0,
  b = Inf,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

ptruncinvgauss(q, m, s, a = 0, b = Inf, ...)

ptrunclnorm(
  q,
  meanlog = 0,
  sdlog = 1,
  a = 0,
  b = Inf,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)

ptruncnbinom(
  q,
  size,
  prob,
  mu,
  a = 0,
  b = Inf,
  ...,
  lower.tail = TRUE,
  log.p = FALSE
)
```

## Arguments

- q:

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

- mean:

  mean of parent distribution

- sd:

  standard deviation is parent distribution

- a:

  point of left truncation. For discrete distributions, `a` will be
  included in the support of the truncated distribution.

- b:

  point of right truncation

- shape1:

  positive shape parameter alpha

- shape2:

  positive shape parameter beta

- size:

  target for number of successful trials, or dispersion parameter (the
  shape parameter of the gamma mixing distribution). Must be strictly
  positive, need not be integer.

- prob:

  probability of success on each trial

- lambda:

  mean and var of "parent" distribution

- df:

  degrees of freedom for "parent" distribution

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

## Value

The cumulative probability of y.

## Examples

``` r
ptrunc(0)
#> [1] 0.5
ptrunc(6, family = "gaussian", mean = 5, sd = 10, b = 7)
#> [1] 0.9319271
pnorm(6, mean = 5, sd = 10) # for comparison
#> [1] 0.5398278
```
