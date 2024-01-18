#' @title Cummulative Distribution Function
#' @description Calculates the cumulative probability for a given truncated distribution
#' @note Either the common or the natural parameters must be provided.
#' @param q vector of quantiles
#' @param family distribution family to use
#' @param lower.tail logical; if `TRUE`, probabilities are \eqn{P(X <= x)}{P(X \leq x)} otherwise, \eqn{P(X > x)}.
#' @param ... additional parameters to be passed to the distribution function.
#' @export
#' @return The cummulative probability of y.
#' @examples
#' ptrunc(0)
ptrunc <- function(q, family = "gaussian", lower.tail = TRUE, ...) {
  # Validating ---------------------------------------------------------------
  family <- tolower(family)
  validateFamilyName(family)

  # Determining object class -------------------------------------------------
  parms <- list(...)
  exclude_parms <- c("log.p")
  parms <- parms[!(names(parms) %in% exclude_parms)]
  class(q) <- genrtruncClass(q, family, names(parms))

  # Dispatching to appropriate funcion ---------------------------------------
  UseMethod("ptrunc", q)
}

#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @rdname ptrunc
#' @inheritParams rtrunc
#' @export
ptrunc.normal <- function(
  q, family, lower.tail = TRUE, log.p = FALSE, mean = 0, sd = 1,
  a = -Inf, b = Inf, ...
) {
  p_q <- pnorm(q, mean, sd, lower.tail = TRUE, log.p)
  p_a <- pnorm(a, mean, sd, lower.tail = TRUE, log.p)
  p_b <- pnorm(b, mean, sd, lower.tail = TRUE, log.p)
  if (log.p) {
    p <- log((exp(p_q) - exp(p_a)) / exp(p_b) - exp(p_a))
  } else {
    p <- (p_q - p_a) / p_b - p_a
  }
  if (!lower.tail) {
    p <- ifelse(log.p, log(1 - exp(p)), 1 - p)
  }
  return(p)
}
