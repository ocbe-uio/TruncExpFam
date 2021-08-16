##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Negative Binomial distribution    ##
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

#' @param size target for number of successful trials,
#' or dispersion parameter (the shape parameter of the gamma mixing
#' distribution). Must be strictly positive, need not be integer.
#' @param prob probability of success on each trial
#' @param mu alternative parametrization via mean
#' @rdname rtrunc
#' @export
rtruncnbinom <- rtrunc.nbinom <- function(n, size, prob, mu, a,b=Inf) {
  y <- rinvnbinom(n, size, prob, mu)  # FIXME #55: write function or replace with rnbinom?
  if (!missing(a)) {
    y <- y[y >= a]
  }
  if (!missing(b)) {
    y <- y[y <= b]
  }
  class(y) <- "trunc_nbinom"
  return(y)
}

#' @export
#' @importFrom stats dnbinom pnbinom
#' @rdname dtrunc
#' @param ... size
#' @export
dtruncnbinom <- dtrunc.trunc_nbinom <- function(y, eta, a = 0, b, ...) {
	my.dnbinom <- function(nsize) dnbinom(y, size = nsize, prob = proba)
	my.pnbinom <- function(z, nsize) pnbinom(z, size = nsize, prob = proba)
	proba <- exp(eta)
	dens <- ifelse((y < a) | (y > b), 0, my.dnbinom(...))

	if (!missing(a)) {
		F.a <- my.pnbinom(a - 1, ...)
	} else {
		F.a <- 0
	}
	if (!missing(b)) {
		F.b <- my.pnbinom(b, ...) # TODO: if output doesn't match stats:: equivalent, the issue is probably here.
	} else {
		F.b <- 1
	}
	return(dens / (F.b - F.a))
}

#' @export
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

#' @export
natural2parameters.trunc_nbinom <- function(eta) {
	# eta: The natural parameters in a negative binomial distribution
	# returns (mean,sigma)
	return(c(p = exp(eta)))
}

#' @export
parameters2natural.trunc_nbinom <- function(parms) {
	# parms: The p parameter in a negative binomial distribution
	# returns the natural parameters
	return(eta = log(parms))
}

get.grad.E.T.inv.trunc_nbinom <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta
	p <- exp(eta)
	return(A = (1-p)^2/(r*p))
# Possible solution: adding validateDomain methods to each rtrunc method.# FIXME #41: r not defined. René is looking into this.
}

get.y.seq.trunc_nbinom <- function(y, y.min = 0, y.max, n = 100) {
	mean <- mean(y, na.rm = T)
	var.y <- var(y, na.rm = T)
	lo <- max(round(y.min), 0)
	hi <- min(y.max, round(mean + 10 * sqrt(var.y)))
	return(lo:hi)
}
