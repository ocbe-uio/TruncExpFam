#' @title Probability Density Function
#' @description Calculates the PDF for a given truncated distribution
#' @param y output from rtrunc
#' @param eta Natural parameters
#' @param a lower truncation limit
#' @param b upper truncation limit
#' @export
#' @examples
#' y <- rtrunc(50, mean=5, sd=2)
#' dtrunc(y, eta=c(0, -1))
#'
dtrunc <- function(y, eta, a, b) {
	UseMethod("dtrunc", y)
}

# ======================================================== #
# Aliases for dtrunc methods                               #
# ======================================================== #
#' @rdname dtrunc
#' @export
dtruncbeta <- dtrunc.trunc_beta

#' @rdname dtrunc
#' @param ... size
#' @export
dtruncbinom <- dtrunc.trunc_binomial

#' @rdname dtrunc
#' @export
dtruncchisq <- dtrunc.trunc_chisq

#' @rdname dtrunc
#' @export
dtrunccontbernoulli <- dtrunc.trunc_contbern

#' @rdname dtrunc
#' @export
dtruncexp <- dtrunc.trunc_exp

#' @rdname dtrunc
#' @export
dtruncgamma <- dtrunc.trunc_gamma



