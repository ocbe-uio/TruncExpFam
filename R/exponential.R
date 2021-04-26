## --##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Chi Square distribution    ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##

#' @export
#' @importFrom stats dexp pexp
dtrunc.trunc_exp <- function(y, eta, a = 0, b) {
	# TODO: develop rtrunc.exp?
	rate <- natural2parameters.exp(eta)
	dens <- ifelse((y <= a) | (y > b), 0, dexp(y, rate=rate))
	if (!missing(a)) {
	  F.a <- pexp(a, rate)
	} else {
		F.a <- 0
	}
	if (!missing(b)) {
		F.b <- pexp(b, parm) # FIXME: parm is not defined
	} else {
		F.b <- 1
	}
	return(dens / (F.b - F.a))
}

#' @export
init.parms.trunc_exp <- function(y) {
	# Returns empirical parameter estimate for the rate parameter
	return(mean(y))
}

sufficient.T.trunc_exp <- function(y) {
	return(suff.T = y)
}

average.T.trunc_exp <- function(y) {
	return(mean(y))
}

#' @export
natural2parameters.trunc_exp <- function(eta) {
	# eta: The natural parameters in an exponential distribution distribution
	# returns rate
	return(c(lamda = -eta))
}

#' @export
parameters2natural.trunc_exp <- function(parms) {
	# parms: The parameter lambda in an exponential distribution
	# returns the natural parameters
	return(eta = -parms)
}

get.grad.E.T.inv.trunc_exp <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta
	return(A = eta^2)
}

get.y.seq.trunc_exp <- function(y, y.min = 0, y.max, n = 100) {
	mean <- mean(y, na.rm = T)
	var.y <- var(y, na.rm = T)
	lo <- max(round(y.min), 0)
	hi <- min(y.max, round(mean + 10 * sqrt(var.y)))
	return(	return(seq(lo, hi, length = n))
)
}
