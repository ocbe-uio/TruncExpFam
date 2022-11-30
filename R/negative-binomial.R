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
  nsize <- attr(y, "parameters")$size
  proba <- attr(y, "parameters")$prob
  my.dnbinom <- function(y, nsize, proba) dnbinom(y, size = nsize, prob = proba)
  my.pnbinom <- function(z, nsize, proba) pnbinom(z, size = nsize, prob = proba)
  dens <- ifelse((y < a) | (y > b), 0, my.dnbinom(y, nsize, proba))
  F.a <- my.pnbinom(a - 1, nsize, proba)
  F.b <- my.pnbinom(b, nsize, proba)
  return(dens / (F.b - F.a))
}

#' @export
empiricalParameters.trunc_nbinom <- function(y, r, k, ...) {
  # Returns empirical parameter estimate for lambda
  if (missing(r) || missing(k)) {
    parms <- c("mean" = mean(y))
  } else {
    parms <- c("size" = r, "prob" = (r - 1) / (r + k - 1))
  }
  class(parms) <- "trunc_nbinom"
  return(parms)
}

sufficientT.trunc_nbinom <- function(y) {
  return(suff.T = y)
}

#' @export
natural2parameters.trunc_nbinom <- function(eta) {
  # eta: The natural parameters in a negative binomial distribution
  p <- c(mean = exp(eta))
  class(p) <- class(eta)
  return(p)
}

#' @export
parameters2natural.trunc_nbinom <- function(parms) {
  # parms: The p parameter in a negative binomial distribution
  # returns the natural parameters
  if (all(names(parms) == c("size", "prob"))) {
    mean <- parms[["size"]] * (1 - parms[["prob"]]) / parms[["prob"]]
  } else {
    mean <- parms[["mean"]]
  }
  eta <- prepEta(log(mean), class(parms))
  return(eta)
}

getGradETinv.trunc_nbinom <- function(eta, r = 1e3, ...) {
  # eta: Natural parameter
  # return the inverse of E.T differentiated with respect to eta
  p <- exp(eta)
  r <- exp(r)
  A <- (1 - p) ^ 2 / (r * p)
  return(A)
}

getYseq.trunc_nbinom <- function(y, y.min = 0, y.max, n = 100) {
  mean <- mean(y, na.rm = TRUE)
  var.y <- var(y, na.rm = TRUE)
  lo <- max(round(y.min), 0)
  hi <- min(y.max, round(mean + 10 * sqrt(var.y)))
  out <- seq(lo, hi)
  attributes(out) <- attributes(y)
  return(out)
}
