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
  q_T <- qnorm(runif(n) * (F_b - F_a) + F_a, mean, sd)
  class(q_T) <- paste0("trunc_", class(n))
  q_T <- attachDistroAttributes(q_T, class(n), c(parms, "a" = a, "b" = b)) # FIXME: parms is global
  return(q_T)
}

#' @export
#' @importFrom stats qbeta
rtrunc_direct.beta <- function(n, family, shape1, shape2, a = 0, b = 1, ...) {
  F_a <- cumDens(a, pbeta, shape1, shape2)
  F_b <- cumDens(b, pbeta, shape1, shape2)
  q_T <- qbeta(runif(n) * (F_b - F_a) + F_a, shape1, shape2)
  class(q_T) <- paste0("trunc_", class(n))
  q_T <- attachDistroAttributes(q_T, class(n), c(parms, "a" = a, "b" = b)) # FIXME: parms is global
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
