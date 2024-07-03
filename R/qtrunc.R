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
  p, mean = 0, sd = 1, a = -Inf, b = Inf, ..., lower.tail, log.p, numeric = FALSE
  ) {
  if (numeric) {
    # Implements a simple bisection algorithm to find the quantile
    lower <- rep(ifelse(a == -Inf, -.Machine$double.xmax, a), length(p))
    upper <- rep(ifelse(b == +Inf, +.Machine$double.xmax, b), length(p))
    q <- rowMeans(cbind(lower, upper))
    p_q <- ptrunc.normal(q, mean, sd, a, b, lower.tail = lower.tail, log.p = log.p)
    tol <- abs(p_q - p)
    for (tl in seq_along(p)) {
      iter <- 0L
      while (tol[tl] > 1e-10 && iter < 1e9) {
        trigger <- ifelse(lower.tail, p_q[tl] < p[tl], p_q[tl] > p[tl])
        if (trigger) {
          lower[tl] <- q[tl]
          q[tl] <- mean(c(q[tl], upper[tl]))
        } else {
          upper[tl] <- q[tl]
          q[tl] <- mean(c(lower[tl], q[tl]))
        }
        p_q[tl] <- ptrunc.normal(
          q[tl], mean, sd, a, b, lower.tail = lower.tail, log.p = log.p
        )
        tol[tl] <- abs(p_q[tl] - p[tl])
        iter <- iter + 1L
      }
    }
  } else {
    F_a <- pnorm(a, mean, sd, lower.tail, FALSE)
    F_b <- pnorm(b, mean, sd, lower.tail, FALSE)
    rescaled_p <- rescale_p(p, F_a, F_b, lower.tail, log.p)
    q <- qnorm(rescaled_p, mean, sd, lower.tail, FALSE)
  }
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
