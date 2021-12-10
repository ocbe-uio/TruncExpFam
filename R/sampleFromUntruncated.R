# this is an internal function used by each rtrunc* function to generate initial
# values
sampleFromTruncated <- function(parms) {

  # Generating empty structure with correct class ------------------------------
  className <- class(parms$n)
  family <- gsub("trunc_", "", className)
  y <- structure(numeric(0), class=className)

  # Checking domain and parmeters -------------------------------------------------
  validateDomain(y, parms)
  if (family %in% c("gamma", "invgamma")) {
    parms$rate <- NULL
  }
  validateFamilyParms(family, names(parms)[!(names(parms) %in% c("a", "b", "n"))])

  # Sampling from untruncated distro -------------------------------------------
  y <- with(parms, switch(class(n),
    "trunc_beta" = rbeta(n, shape1, shape2),
    "trunc_binomial" = rbinom(n, size, prob),
    "trunc_chisq" = rchisq(n, df),
    "trunc_contbern" = rcontbern(n, lambda),
    "trunc_exp" = rexp(n, rate),
    "trunc_gamma" = rgamma(n, shape = shape, scale = scale),
    "trunc_invgamma" = rinvgamma(n, shape = shape, scale = scale),
    "trunc_invgauss" = rinvgauss(n, m, s),
    "trunc_lognormal" = rlnorm(n, meanlog, sdlog),
    "trunc_nbinom" = rnbinom(n, size, prob, mu),
    "trunc_normal" = rnorm(n, mean, sd),
    "trunc_poisson" = rpois(n, lambda)
  ))

  # Truncating -----------------------------------------------------------------
  y <- y[y >= parms$a]
  y <- y[y <= parms$b]

  # Attaching attributes -------------------------------------------------------
  class(y) <- className
  y <- attachDistroAttributes(y, family, parms)
  return(y)
}

# Sampling function for a continuous bernoulli distribution
# This distribution is not implemented in Base R
# Used in the sampling of the truncated continuous bernoulli
rcontbern <- function(n, lambda){
  if ((lambda < 0) | (lambda > 1)) {
    stop("lambda must be in (0, 1)")
  }
  u <- runif(n)
  if (lambda == 0.5) {
    return(u)
  }
  x <- log(1 + (2 * lambda - 1) * u / (1 - lambda)) / (log(lambda / (1 - lambda))) # The inverse of the CDF for a cont. bernoulli distribution
  return(x)
}
