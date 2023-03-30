#' @rdname rtrunc
#' @export
rtrunc_direct <- function(n, family = "gaussian", ...) {

  # Validating ---------------------------------------------------------------
  family <- tolower(family)
  validateFamilyName(family)

  # Determining object class -------------------------------------------------
  parms <- list(...)
  class(n) <- genrtruncClass(n, family, names(parms))

  # Dispatching to appropriate sampling funcion ------------------------------
  UseMethod("rtrunc_direct", n)
}

#' @export
rtrunc_direct.normal <- function(n, family, mean = 0, sd = 1, a = -Inf, b = Inf, ...) {
  F_a <- cumDens(a, pnorm, mean, sd)
  F_b <- cumDens(b, pnorm, mean, sd)
  q_T <- truncated_q(qnorm(rescaled_q(n, F_a, F_b), mean, sd), mget(ls()))
  return(q_T)
}

#' @export
rtrunc_direct.beta <- function(n, family, shape1, shape2, a = 0, b = 1, ...) {
  F_a <- cumDens(a, pbeta, shape1, shape2)
  F_b <- cumDens(b, pbeta, shape1, shape2)
  q_T <- truncated_q(qbeta(rescaled_q(n, F_a, F_b), shape1, shape2), mget(ls()))
  return(q_T)
}

#' @export
rtrunc_direct.chisq <- function(n, family, df, a = 0, b = Inf, ...) {
  F_a <- cumDens(a, pchisq, df)
  F_b <- cumDens(b, pchisq, df)
  q_T <- truncated_q(qchisq(rescaled_q(n, F_a, F_b), df), mget(ls()))
  return(q_T)
}

#' @export
rtrunc_direct.contbern <- function(n, family, lambda, a = 0, b = 1, ...) {
  F_a <- cumDens(a, pcontbern, lambda)
  F_b <- cumDens(b, pcontbern, lambda)
  q_T <- truncated_q(qcontbern(rescaled_q(n, F_a, F_b), lambda), mget(ls()))
  return(q_T)
}

#' @export
rtrunc_direct.exp <- function(n, family, rate, a = 0, b = Inf, ...) {
  F_a <- cumDens(a, pexp, rate)
  F_b <- cumDens(b, pexp, rate)
  q_T <- truncated_q(qexp(rescaled_q(n, F_a, F_b), rate), mget(ls()))
  return(q_T)
}

#' @export
rtrunc_direct.gamma <- function(
  n, family, shape, rate = 1, scale = 1 / rate, a = 0, b = Inf, ...
) {
  F_a <- cumDens(a, pgamma, rate, scale)
  F_b <- cumDens(b, pgamma, rate, scale)
  q_T <- truncated_q(qgamma(rescaled_q(n, F_a, F_b), rate, scale), mget(ls()))
  return(q_T)
}

cumDens <- function(x, probFunction, ...) {
  if (x == -Inf || x == 0) {
    return(0)
  } else if (x == Inf || x == 1) {
    return(1)
  } else {
    return(probFunction(x, ...))
  }
}

truncated_q <- function(q_T, parms) {
  class(q_T) <- paste0("trunc_", class(parms[["n"]]))
  q_T <- attachDistroAttributes(
    sample = q_T,
    family = class(parms[["n"]]),
    parms  = c(parms$parms, parms["a"], parms["b"])
  )
  return(q_T)
}

rescaled_q <- function(n, F_a, F_b) {
  return(runif(n) * (F_b - F_a) + F_a)
}
