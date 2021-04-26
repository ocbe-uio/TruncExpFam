##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Negative Binomial distribution    ##
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

#' @importFrom stats dnbinom pnbinom
dtrunc.trunc_nbinom <- function(y, eta, a = 0, b, ...) {
	# TODO: develop rtrunc.nbinom
  my.dnbinom <- function(nsize) {
    dnbinom(y, size = nsize, prob = proba)
  }
  my.pnbinom <- function(z, nsize) {
    pnbinom(z, size = nsize, prob = proba)
  }
  proba <- exp(eta)
  dens <- ifelse((y < a) | (y > b), 0, my.dnbinom(...))

	if (!missing(a)) {
		F.a <- my.pbinom(a - 1, ...)
	} else {
		F.a <- 0
	}
	if (!missing(b)) {
		F.b <- my.pbinom(b, ...)
	} else {
		F.b <- 1
	}
	return(dens / (F.b - F.a))
}

init.parms.trunc_nbinom <- function(y) {
	# Returns empirical parameter estimate for lambda
	return(mean(y))
}

sufficient.T.trunc_nbinom <- function(y) {
	return(suff.T = y)
}

average.T.trunc_nbinom <- function(y) {
	return(mean(y))
}

natural2parameters.trunc_nbinom <- function(eta) {
	# eta: The natural parameters in a negative binomial distribution
	# returns (mean,sigma)
	return(c(p = exp(eta)))
}

parameters2natural.trunc_nbinom <- function(parms) {
	# parms: The p parameter in a negative binomial distribution
	# returns the natural parameters
	return(eta = log(parms))
}

get.grad.E.T.inv.trunc_nbinom <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta
  p=exp(eta)
	return(A = (1-p)^2/(r*p)) # FIXME: r not defined
}

get.y.seq.trunc_nbinom <- function(y, y.min = 0, y.max, n = 100) {
	mean <- mean(y, na.rm = T)
	var.y <- var(y, na.rm = T)
	lo <- max(round(y.min), 0)
	hi <- min(y.max, round(mean + 10 * sqrt(var.y)))
	return(lo:hi)
}
