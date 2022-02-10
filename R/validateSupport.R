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
}

validateSupport.trunc_contbern <- function(n, parms, ...) {
}

validateSupport.trunc_exp <- function(n, parms, ...) {
<<<<<<< HEAD
  support <- createSupport(0, Inf, "[)")
  judgeSupportLimits(parms, support)
=======
  domain <- createDomain(0, Inf, "[)")
  judgeDomainLimits(parms, domain)
>>>>>>> 90d1191 (Made domain creation + validation functions (#73))
}

validateSupport.trunc_gamma <- function(n, parms, ...) {
}

validateSupport.trunc_invgamma <- function(n, parms, ...) {
}

validateSupport.trunc_invgauss <- function(n, parms, ...) {
}

validateSupport.trunc_lognormal <- function(n, parms, ...) {
}

validateSupport.trunc_nbinom <- function(n, parms, ...) {
}

validateSupport.trunc_normal <- function(n, parms, ...) {
}

validateSupport.trunc_poisson <- function(n, parms, ...) {
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

judgeSupportLimits <- function(parms, support) {
  if(parms$a == parms$b) {
    stop("Identical truncation limits: a = b = ", parms$a)
  } else if (parms$a >= support$u | parms$b <= support$l) {
    stop("Truncation limits (a, b) must be a subset of ", support$txt)
  } else if (parms$a < support$l | parms$b > support$u) {
    warning("Truncation limits (a, b) are not a subset of ", support$txt)
  } else if (parms$b <= parms$a) {
    stop("Upper truncation limit (b) must be higher than lower limit (a)")
  }
}
