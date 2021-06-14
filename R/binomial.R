## --##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Binomial distribution  ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##

rtrunc.binomial <- function(n, size, prob, a, b) {
# TODO: size needs to be handled as a 'fixed' parameter  
	y <- rbinom(n, size, prob)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	}
	class(y) <- "trunc_binomial"
	return(y)
}

#' @export
dtrunc.trunc_binomial <- function(y, eta, a = 0, b, ...) {
	my.dbinom <- function(nsize) {
		dbinom(y, size = nsize, prob = proba)
	}
	my.pbinom <- function(z, nsize) {
		pbinom(z, size = nsize, prob = proba)
	}
	proba <- 1 / (1 + exp(-eta))
	dens <- ifelse((y < a) | (y > b), 0, my.dbinom(...))
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

#' @export
init.parms.trunc_binomial <- function(y, ...) {
	# Returns empirical parameter estimate for lambda
	parms <- mean(y / ...) # FIXME: should be y / size (#19)
	class(parms) <- "trunc_binomial"
	return(parms)
}

sufficient.T.trunc_binomial <- function(y) {
	return(suff.T = y)
}

average.T.trunc_binomial <- function(y) {
	return(mean(y))
}

density.trunc_binomial <- function(y, eta, ...) {
	parms <- 1 / (1 + exp(-eta))
	dbinom(y, ..., parms)
}

#' @export
natural2parameters.trunc_binomial <- function(eta) {
	# eta: The natural parameters in a binomial distribution
	# returns (p)
	return(p = 1 / (1 + exp(-eta)))
}

#' @export
parameters2natural.trunc_binomial <- function(parms) {
	# parms: The probability parameter p in a binomial distribution
	# returns the natural parameters
	return(eta = log(parms / (1 - parms)))
}

get.grad.E.T.inv.trunc_binomial <- function(eta, ...) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta
	exp.eta <- exp(eta)
	return(A = ((1 + exp.eta)^2 / exp.eta) / ...)
}

get.y.seq.trunc_binomial <- function(y, y.min = 0, y.max, n = 100, ...) {
	nsize <- 0 + ...
	y.lo <- round(y.min)
	y.hi <- round(y.max)
	lo <- max(y.lo, 0)
	hi <- min(y.max, nsize)
	return(lo:hi)
}
