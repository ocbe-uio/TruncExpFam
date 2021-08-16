#' @title Probability Density Function
#' @description Calculates the PDF for a given truncated distribution
#' @param y output from rtrunc
#' @param eta Natural parameters
#' @param a lower truncation limit
#' @param b upper truncation limit
#' @export
#' @examples
#' y <- rtrunc(50, mean = 5, sd = 2)
#' dtrunc(y, eta = c(0, -1))
dtrunc <- function(y, eta, a, b) {
	UseMethod("dtrunc", y)
}

# ======================================================== #
# Aliases for dtrunc methods                               #
# ======================================================== #

# FIXME #47: No idea why R can't find the functions below. >:(

# #' @rdname dtrunc
# #' @export
# dtruncexp <- dtrunc.trunc_exp

# #' @rdname dtrunc
# #' @export
# dtruncgamma <- dtrunc.trunc_gamma

# #' @rdname dtrunc
# #' @export
# dtruncinvgamma <- dtrunc.trunc_invgamma

# #' @rdname dtrunc
# #' @export
# dtruncinvgauss <- dtrunc.trunc_invgauss

# #' @rdname dtrunc
# #' @export
# dtrunclnorm <- dtrunc.trunc_lognormal

# #' @rdname dtrunc
# #' @param ... size
# #' @export
# dtruncnbinom <- dtrunc.trunc_nbinom

# #' @rdname dtrunc
# #' @export
# dtruncnorm <- dtrunc.trunc_normal

# #' @rdname dtrunc
# #' @export
# dtruncpois <- dtrunc.trunc_poisson