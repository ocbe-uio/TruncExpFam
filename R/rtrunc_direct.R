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
#' @importFrom stats qnorm
rtrunc_direct.normal <- function(n, family, mean = 0, sd = 1, a = -Inf, b = Inf, ...) {
  F_a <- cumDens(a, pnorm, mean, sd)
  F_b <- cumDens(b, pnorm, mean, sd)
  q_T <- truncated_q(qnorm(runif(n) * (F_b - F_a) + F_a, mean, sd), mget(ls()))
  return(q_T)
}

#' @export
#' @importFrom stats qbeta
rtrunc_direct.beta <- function(n, family, shape1, shape2, a = 0, b = 1, ...) {
  F_a <- cumDens(a, pbeta, shape1, shape2)
  F_b <- cumDens(b, pbeta, shape1, shape2)
  q_T <- truncated_q(qbeta(runif(n) * (F_b - F_a) + F_a, shape1, shape2), mget(ls()))
  return(q_T)
}

#' @export
#' @importFrom stats qchisq
rtrunc_direct.chisq <- function(n, family, df, a = 0, b = Inf, ...) {
  F_a <- cumDens(a, pchisq, df)
  F_b <- cumDens(b, pchisq, df)
  q_T <- truncated_q(qchisq(runif(n) * (F_b - F_a) + F_a, df), mget(ls()))
  return(q_T)
}

#' @export
rtrunc_direct.contbern <- function(n, family, lambda, a = 0, b = 1, ...) {
  F_a <- cumDens(a, pcontbern, lambda)
  F_b <- cumDens(b, pcontbern, lambda)
  q_T <- truncated_q(qcontbern(runif(n) * (F_b - F_a) + F_a, lambda), mget(ls()))
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
