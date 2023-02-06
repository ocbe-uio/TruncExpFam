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
  p <- runif(n)
  F_a <- cumDens(a, pnorm, mean, sd)
  F_b <- cumDens(b, pnorm, mean, sd)
  return(qnorm(p * (F_b - F_a) + F_a, mean, sd))
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
