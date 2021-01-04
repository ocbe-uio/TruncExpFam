## --##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Poisson distribution   ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##

#' @title Random Truncated Poisson
#' @param n sample size
#' @param lambda mean and var of "parent" distribution
#' @param a point of left truncation
#' @param b point of right truncation
#' @return A sample of size n drawn from a truncated Poisson distribution
#' @note The effective sample size is reduced due to truncation. a, and b are included in the domain
#' @author René Holst
#' @examples
#' sample.pois <- rtrunc.pois(1000, 10, 4)
#' hist(sample.pois)
#' @export
rtrunc.pois <- function(n, lambda, a, b) {
	y <- rpois(n, lambda)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	}
	return(y)
}

density.trunc.pois <- function(y, eta, a = 0, b) {
	parm <- exp(eta)
	dens <- ifelse((y < a) | (y > b), 0, dpois(y, parm))
	if (!missing(a)) {
		F.a <- ppois(a - 1, parm)
	} else {
		F.a <- 0
	}
	if (!missing(b)) {
		F.b <- ppois(b, parm)
	} else {
		F.b <- 1
	}
	return(dens / (F.b - F.a))
}

init.parms.pois <- function(y) {
	# Returns empirical parameter estimate for lambda
	parm <- mean(y) + 5
}

sufficient.T.pois <- function(y) {
	return(suff.T = y)
}

average.T.pois <- function(y) {
	return(mean(y))
}

density.pois <- function(y, eta) {
	parms <- exp(eta)
	dpois(y, parms)
}

natural2parameters.pois <- function(eta) {
	# eta: The natural parameters in a Poisson distribution
	# returns (mu,sigma)
	return(lambda = exp(eta))
}

parameters2natural.pois <- function(parms) {
	# parms: The parameter lambda in a Poisson distribution
	# returns the natural parameters
	return(eta = log(parms))
}

get.grad.E.T.inv.pois <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta
	return(A = exp(-eta))
}

get.y.seq.pois <- function(y, y.min = 0, y.max, n = 100) {
	mu <- mean(y, na.rm = T)
	var.y <- var(y, na.rm = T)
	lo <- max(round(y.min), 0)
	hi <- min(y.max, round(mu + 10 * sqrt(var.y)))
	return(lo:hi)
}
