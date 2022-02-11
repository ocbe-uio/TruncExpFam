validateSupport <- function(n, ...) {
  UseMethod("validateSupport")
}

validateSupport.trunc_beta <- function(n, parms, ...) {
  support <- createSupport(0, 1, c("[]", "()"))
  judgeSupportLimits(parms, support)
}

validateSupport.trunc_binomial <- function(n, parms, ...) {
  support <- createSupport(0, parms$size, "[]")
  judgeSupportLimits(parms, support)
}

validateSupport.trunc_chisq <- function(n, parms, ...) {
  if (parms$df == 1) {
    support <- createSupport(0, Inf, "()")
  } else {
    support <- createSupport(0, Inf, "[)")
  }
  judgeSupportLimits(parms, support)
}

validateSupport.trunc_contbern <- function(n, parms, ...) {
  support <- createSupport(0, 1, "[]")
  judgeSupportLimits(parms, support)
}

validateSupport.trunc_exp <- function(n, parms, ...) {
  support <- createSupport(0, Inf, "[)")
  judgeSupportLimits(parms, support)
}

validateSupport.trunc_gamma <- function(n, parms, ...) {
  support <- createSupport(0, Inf, "()")
  judgeSupportLimits(parms, support)
}

validateSupport.trunc_invgamma <- function(n, parms, ...) {
  support <- createSupport(0, Inf, "()")
  judgeSupportLimits(parms, support)
}

validateSupport.trunc_invgauss <- function(n, parms, ...) {
  support <- createSupport(0, Inf, "()")
  judgeSupportLimits(parms, support)
}

validateSupport.trunc_lognormal <- function(n, parms, ...) {
  support <- createSupport(0, Inf, "()")
  judgeSupportLimits(parms, support)
}

validateSupport.trunc_nbinom <- function(n, parms, ...) {
  support <- createSupport(0, Inf, "[)")
  judgeSupportLimits(parms, support)
}

validateSupport.trunc_normal <- function(n, parms, ...) {
  support <- createSupport(-Inf, Inf, "()")
  judgeSupportLimits(parms, support, TRUE)
}

validateSupport.trunc_poisson <- function(n, parms, ...) {
  support <- createSupport(0, Inf, "[)")
  judgeSupportLimits(parms, support)
}

createSupport <- function(lower, upper, inclusion_brackets) {
  out <- list(
    l = lower,
    u = upper,
    txt = vector("character")
  )
  split_brackets <- strsplit(inclusion_brackets, "")
  for (i in seq_along(split_brackets)) {
    out$txt <- append(
      out$txt,
      paste0(split_brackets[[i]][1], out$l, ", ", out$u, split_brackets[[i]][2])
    )
  }
  out$txt <- paste(out$txt, collapse = " or ")
  return(out)
}

judgeSupportLimits <- function(parms, support, no_complex = FALSE) {
  if (no_complex & (is.complex(parms$a) | is.complex(parms$b))) {
    stop("Truncation limits may not contain complex numbers")
  }
  # TODO: improve >= and <= to consider sign of support, ( or [
  if (parms$a == parms$b) {
    stop("Identical truncation limits: a = b = ", parms$a)
  } else if (parms$a >= support$u | parms$b <= support$l) {
    stop("Truncation limits (a, b) must be a subset of ", support$txt)
  } else if (parms$a < support$l | parms$b > support$u) {
    warning(
      "Truncation limits (", parms$a, ", ", parms$b, ") are not a subset of ",
      support$txt
    )
  } else if (parms$b <= parms$a) {
    stop("Upper truncation limit (b) must be higher than lower limit (a)")
  }
}
