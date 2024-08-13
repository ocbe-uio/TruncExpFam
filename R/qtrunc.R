#' @title Quantile Function
#' @description Calculates quantile for a given truncated distribution and
#' probability.
#' @param p vector of quantiles
#' @param family distribution family to use
#' @param lower.tail logical; if `TRUE`, probabilities are
#' \eqn{P(X <= x)}{P(X \leq x)} otherwise, \eqn{P(X > x)}
#' @param log.p logical; if `TRUE`, probabilities p are given as `log(p)`
#' @param ... \emph{named} distribution parameters and/or truncation limits
#' (`a`, `b`)
#' @export
#' @return The quantile of `p`.
#' @examples
#' qtrunc(0.75)
#' qtrunc(.2, family = "gaussian", mean = 5, sd = 10, b = 7)
#' qnorm(.2, mean = 5, sd = 10) # for comparison
qtrunc <- function(p, family, ..., lower.tail = TRUE, log.p = FALSE) {
  # This is a pseudo-generic function to pre-process arguments and call the
  # actual generic, qtrunc.generic().

  # Validating ---------------------------------------------------------------
  if (missing(family)) family <- "gaussian"
  family <- tolower(family)
  validateFamilyName(family)

  # Reclassifying p and dispatching -------------------------------------------
  class(p) <- genrtruncClass(p, family, NULL)
  quant <- qtrunc.generic(p, ..., lower.tail = lower.tail, log.p = log.p)
  return(unclass(quant))
}

qtrunc.generic <- function(p, ..., lower.tail, log.p) {
  UseMethod("qtrunc", p)
}

qtrunc.beta <- function(
    p, shape1, shape2, a = 0, b = 1, ..., lower.tail, log.p
  ) {
  F_a <- pbeta(a, shape1, shape2, ncp = 0, lower.tail, FALSE)
  F_b <- pbeta(b, shape1, shape2, ncp = 0, lower.tail, FALSE)
  rescaled_p <- rescale_p(p, F_a, F_b, lower.tail, log.p)
  q <- qbeta(rescaled_p, shape1, shape2, ncp = 0, lower.tail, FALSE)
  return(q)
}

qtrunc.binomial <- function(
    p, size, prob, a = 0, b = size, ..., lower.tail, log.p
  ) {
  F_a <- pbinom(a - 1L, size, prob, lower.tail, FALSE)
  F_b <- pbinom(b, size, prob, lower.tail, FALSE)
  rescaled_p <- rescale_p(p, F_a, F_b, lower.tail, log.p)
  q <- qbinom(rescaled_p, size, prob, lower.tail, FALSE)
  return(q)
}

qtrunc.chisq <- function(p, df, a = 0, b = Inf, ..., lower.tail, log.p) {
  F_a <- pchisq(a - 1L, df, ncp = 0, lower.tail, FALSE)
  F_b <- pchisq(b, df, ncp = 0, lower.tail, FALSE)
  rescaled_p <- rescale_p(p, F_a, F_b, lower.tail, log.p)
  q <- qchisq(rescaled_p, df, ncp = 0, lower.tail, FALSE)
  return(q)
}

qtrunc.contbern <- function(p, lambda, a = 0, b = 1, ..., lower.tail, log.p) {
  F_a <- pcontbern(a, lambda)
  F_b <- pcontbern(b, lambda)
  rescaled_p <- rescale_p(p, F_a, F_b, lower.tail, log.p)
  q <- qcontbern(rescaled_p, lambda)
  return(q)
}

qtrunc.exp <- function(p, rate = 1, a = 0, b = Inf, ..., lower.tail, log.p) {
  F_a <- pexp(a, rate, lower.tail, FALSE)
  F_b <- pexp(b, rate, lower.tail, FALSE)
  rescaled_p <- rescale_p(p, F_a, F_b, lower.tail, log.p)
  q <- qexp(rescaled_p, rate, lower.tail, FALSE)
  return(q)
}

qtrunc.gamma <- function(
  p, shape, rate = 1, scale = 1 / rate, a = 0, b = Inf, ..., lower.tail, log.p
) {
  F_a <- pgamma(a, shape, scale = scale, lower.tail = lower.tail, log.p = FALSE)
  F_b <- pgamma(b, shape, scale = scale, lower.tail = lower.tail, log.p = FALSE)
  rescaled_p <- rescale_p(p, F_a, F_b, lower.tail, log.p)
  q <- qgamma(rescaled_p, shape, scale = scale, lower.tail = lower.tail, log.p = FALSE)
  return(q)
}

qtrunc.invgamma <- function(
  p, shape, rate = 1, scale = 1 / rate, a = 0, b = Inf, ..., lower.tail, log.p
) {
  F_a <- pinvgamma(a, shape, scale = scale, lower.tail = lower.tail, log.p = FALSE)
  F_b <- pinvgamma(b, shape, scale = scale, lower.tail = lower.tail, log.p = FALSE)
  rescaled_p <- rescale_p(p, F_a, F_b, lower.tail, log.p)
  q <- qinvgamma(rescaled_p, shape, scale = scale, lower.tail = lower.tail, log.p = FALSE)
  return(q)
}

qtrunc.invgauss <- function(p, m, s, a = 0, b = Inf, ..., lower.tail, log.p) {
  if (!lower.tail || log.p) {
    stop("Only lower.tail = TRUE and log.p = FALSE are supported.")
  }
  F_a <- ifelse(a == 0, 0, pinvgauss(a, m, s))
  F_b <- ifelse(b == Inf, 1, pinvgauss(b, m, s))
  rescaled_p <- rescale_p(p, F_a, F_b, lower.tail, log.p)
  q <- qinvgauss(rescaled_p, m, s)
  return(q)
}

qtrunc.lognormal <- function(
    p, meanlog = 0, sdlog = 1, a = 0, b = Inf, ..., lower.tail, log.p
  ) {
  F_a <- plnorm(a, meanlog, sdlog, lower.tail, FALSE)
  F_b <- plnorm(b, meanlog, sdlog, lower.tail, FALSE)
  rescaled_p <- rescale_p(p, F_a, F_b, lower.tail, log.p)
  q <- qlnorm(rescaled_p, meanlog, sdlog, lower.tail, FALSE)
  return(q)
}

qtrunc.normal <- function(
    p, mean = 0, sd = 1, a = -Inf, b = Inf, ..., lower.tail, log.p
  ) {
  F_a <- pnorm(a, mean, sd, lower.tail, FALSE)
  F_b <- pnorm(b, mean, sd, lower.tail, FALSE)
  rescaled_p <- rescale_p(p, F_a, F_b, lower.tail, log.p)
  q <- qnorm(rescaled_p, mean, sd, lower.tail, FALSE)
  return(q)
}

rescale_p <- function(p, F_a, F_b, lower.tail, log.p) {
  if (log.p) {
    p <- exp(p)
  }
  if (lower.tail) {
    p <- p * F_b + (1 - p) * F_a
  } else  {
    p <- p * F_a + (1 - p) * F_b
  }
  return(p)
}
