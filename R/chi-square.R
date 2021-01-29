## --##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Chi Square distribution    ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##

density.trunc.chisq <- function(y, eta, a, b) {
	parm <- natural2parameters.chisq(eta)
	dens <- ifelse((y < a) | (y > b), 0, dchisq(y, shape = parm[1], rate = parm[2]))
	if (!missing(a)) {
		F.a <- pgamma(a, shape = parm[1], rate = parm[2])
	} else {
		F.a <- 0
	}
	if (!missing(b)) {
		F.b <- pgamma(b, shape = parm[1], rate = parm[2])
	} else {
		F.b <- 1
	}
	const <- 1 / (F.b - F.a)
	return(dens / (F.b - F.a))
}

init.parms.gamma <- function(y) {
	# Returns  parameter estimates mu and sd
	amean <- mean(y)
	avar <- var(y)
	a <- amean^2 / avar
	parm <- c(alpha = a, beta = a / amean)
}

sufficient.T.gamma <- function(y) {
	return(suff.T = cbind(log(y), y))
}

average.T.gamma <- function(y) {
	return(apply(cbind(log(y), y), 2, mean))
}

natural2parameters.gamma <- function(eta) {
	# eta: The natural parameters in a gamma distribution
	# returns (alpha,beta)
	return(c(alpha = eta[1] + 1, beta = -eta[2]))
}

#' @title Convert parameters to Natural Gamma
#' @param parms The parameters alpha and beta in a gamma distribution
#' @return The natural parameters
#' @author René Holst
#' @examples
#' sample.lognorm <- rtrunc(n = 100000, mulog = 2.5, sigmalog = 0.5, a = 7)
#' ml_lognormal <- ml.estimation.trunc.dist(
#'   sample.lognorm, y.min = 7, max.it = 500, tol = 1e-10, delta = 0.3,
#'   family = "LogNormal"
#' )
#' eta.hat <- parameters2natural.gamma(ml_lognormal)
#' @export
parameters2natural.gamma <- function(parms) {
	# parms: The parameters alpha and beta in a gamma distribution
	# returns the natural parameters
	return(c(eta.1 = parms[1] - 1, eta.2 = -parms[2]))
}

get.y.seq.gamma <- function(y, y.min = 1e-6, y.max, n = 100) {
	# Bør chekkes
	mu <- mean(y, na.rm = T)
	sd <- var(y, na.rm = T)^0.5
	lo <- max(y.min, mu - 5 * sd)
	hi <- min(y.max, mu + 5 * sd)
	return(seq(lo, hi, length = n))
}

get.grad.E.T.inv.gamma <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta' : p x p matrix
	return(A = solve(matrix(c(-1 / eta[1]^2 + dpsi.dx(eta[1]), -1 / eta[2], -1 / eta[2], (eta[1] + 1) / eta[2]^2), ncol = 2)))
}
