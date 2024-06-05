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

qtrunc.normal <- function(
  p, mean = 0, sd = 1, a = -Inf, b = Inf, ..., lower.tail, log.p
  ) {
  lower <- ifelse(a == -Inf, -.Machine$double.xmax, a)
  upper <- ifelse(b == +Inf, +.Machine$double.xmax, b)
  q <- mean(c(lower, upper))
  p_q <- ptrunc.normal(q, mean, sd, a, b, lower.tail = lower.tail, log.p = log.p)
  tol <- abs(p_q - p)
  while (any(tol > 1e-10)) {
    if (p_q < p) {
      lower <- q
    } else {
      upper <- q
    }
    q <- ifelse(p_q < p, mean(c(q, upper)), mean(c(lower, q)))
    p_q <- ptrunc.normal(q, mean, sd, a, b, lower.tail = lower.tail, log.p = log.p)
    tol <- abs(p_q - p)
  }
  return(q)
}
