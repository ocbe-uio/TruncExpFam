#' @title Cummulative Distribution Function
#' @description Calculates the cumulative probability for a given truncated distribution
#' @param q vector of quantiles
#' @param family distribution family to use
#' @param lower.tail logical; if `TRUE`, probabilities are \eqn{P(X <= x)}{P(X \leq x)} otherwise, \eqn{P(X > x)}
#' @param log.p logical; if `TRUE`, probabilities p are given as `log(p)`
#' @param ... \emph{named} distribution parameters and/or truncation limits (`a`, `b`)
#' @export
#' @return The cummulative probability of y.
#' @examples
#' ptrunc(0)
#' ptrunc(6, family = "gaussian", mean = 5, sd = 10, b = 7)
#' pnorm(6, mean = 5, sd = 10) # for comparison
ptrunc <- function(q, family, ..., lower.tail = TRUE, log.p = FALSE) {
  # This is a pseudo-generic function to pre-process arguments and call the
  # actual generic, ptrunc.generic().

  # Validating ---------------------------------------------------------------
  if (missing(family)) family <- "gaussian"
  family <- tolower(family)
  validateFamilyName(family)

  # Reclassifying q and dispatching -------------------------------------------
  class(q) <- genrtruncClass(q, family, NULL)
  prob <- ptrunc.generic(q, ..., lower.tail = lower.tail, log.p = log.p)
  return(unclass(prob))
}

ptrunc.generic <- function(q, ..., lower.tail, log.p) {
  UseMethod("ptrunc", q)
}

ptrunc.normal <- function(q, mean = 0, sd = 1, a = -Inf, b = Inf, ..., lower.tail, log.p) {
  p_q <- pnorm(q, mean, sd, lower.tail = TRUE, log.p)
  p_a <- pnorm(a, mean, sd, lower.tail = TRUE, log.p)
  p_b <- pnorm(b, mean, sd, lower.tail = TRUE, log.p)
  return(truncated_p(p_q, p_a, p_b, lower.tail, log.p))
}

ptrunc.beta <- function(q, shape1, shape2, a = 0, b = 1, ..., lower.tail, log.p) {
  p_q <- pbeta(q, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p)
  p_a <- pbeta(a, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p)
  p_b <- pbeta(b, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p)
  return(truncated_p(p_q, p_a, p_b, lower.tail, log.p))
}

truncated_p <- function(p_q, p_a, p_b, lower.tail, log.p) {
  if (log.p) {
    p <- log((exp(p_q) - exp(p_a)) / (exp(p_b) - exp(p_a)))
    if (!lower.tail) {
      p <- log(1 - exp(p))
    }
  } else {
    p <- (p_q - p_a) / (p_b - p_a)
    if (!lower.tail) {
      p <- 1 - p
    }
  }
  return(p)
}
