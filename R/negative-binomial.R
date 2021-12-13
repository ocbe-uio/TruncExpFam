## --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Negative Binomial distribution    ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

#' @param size target for number of successful trials,
#' or dispersion parameter (the shape parameter of the gamma mixing
#' distribution). Must be strictly positive, need not be integer.
#' @param prob probability of success on each trial
#' @param mu alternative parametrization via mean
#' @importFrom stats rnbinom
#' @rdname rtrunc
#' @export
rtruncnbinom <- rtrunc.nbinom <- function(n, size, prob, mu, a = 0, b = Inf) {
  class(n) <- "trunc_nbinom"
  sampleFromTruncated(mget(ls()))
}

#' @export
#' @importFrom stats dnbinom pnbinom
#' @rdname dtrunc
#' @param ... size
#' @export
dtruncnbinom <- dtrunc.trunc_nbinom <- function(y, eta, a = 0, b = Inf, ...) {
  my.dnbinom <- function(nsize) dnbinom(y, size = nsize, prob = proba)
  my.pnbinom <- function(z, nsize) pnbinom(z, size = nsize, prob = proba)
  proba <- exp(eta)
  dens <- ifelse((y < a) | (y > b), 0, my.dnbinom(...))

  if (!missing(a)) {
    F.a <- my.pnbinom(a - 1, ...)
  } else {
    F.a <- 0
  }
  if (!missing(b)) {
    F.b <- my.pnbinom(b, ...)
  } else {
    F.b <- 1
  }
  return(dens / (F.b - F.a))
}

#' @export
init.parms.trunc_nbinom <- function(y) {
  # Returns empirical parameter estimate for lambda
  return(mean(y))
}

sufficientT.trunc_nbinom <- function(y) {
  return(suff.T = y)
}

averageT.trunc_nbinom <- function(y) {
  return(mean(y))
}

#' @export
natural2parameters.trunc_nbinom <- function(eta) {
  # eta: The natural parameters in a negative binomial distribution
  # returns (mean,sigma)
  p <- exp(eta)
  class(p) <- class(eta)
  return(p)
}

#' @export
parameters2natural.trunc_nbinom <- function(parms) {
  # parms: The p parameter in a negative binomial distribution
  # returns the natural parameters
  eta <- log(parms)
  class(eta) <- class(parms)
  return(eta)
}

getGradETinv.trunc_nbinom <- function(eta) {
  # eta: Natural parameter
  # return the inverse of E.T differentiated with respect to eta
  p <- exp(eta)
  return(A = (1 - p)^2 / (r * p))
  # Possible solution: adding validateDomain methods to each rtrunc method.# FIXME #41: r not defined. René is looking into this.
}

getYseq.trunc_nbinom <- function(y, y.min = 0, y.max, n = 100) {
  mean <- mean(y, na.rm = T)
  var.y <- var(y, na.rm = T)
  lo <- max(round(y.min), 0)
  hi <- min(y.max, round(mean + 10 * sqrt(var.y)))
  out <- seq(lo, hi)
  class(out) <- class(y)
  return(out)
}
