## --##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Beta distribution      ##
##         Variant 1                                 ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##

#' @export
#' @importFrom stats dbeta pbeta
dtrunc.trunc_beta <- function(y, eta, a, b) {
	parm <- natural2parameters.beta(eta)
	dens <- ifelse((y < a) | (y > b), 0, dbeta(y, shape1 = parm[1], shape2 = parm[2]))
	if (!missing(a)) {
		F.a <- pbeta(a, shape1 = parm[1], shape2 = parm[2])
	} else {
		F.a <- 0
	}
	if (!missing(b)) {
		F.b <- pbeta(b, shape1 = parm[1], shape2 = parm[2])
	} else {
		F.b <- 1
	}
	const <- 1 / (F.b - F.a)
	return(dens / (F.b - F.a))
}

#' @export
init.parms.trunc_beta <- function(y) {
	# Returns  parameter estimates mean and sd
	amean <- mean(y)
	avar <- var(y)
	alpha <- amean^2*(1-amean)/avar-amean
	beta  <- alpha*(1/amean-1)
	return(c(shape1 = alpha, shape2 = beta))
}

sufficient.T.trunc_beta <- function(y) {
	return(suff.T = cbind(log(y), log(1-y)))
}

average.T.trunc_beta <- function(y) {
	return(apply(cbind(log(y), log(1-y)), 2, mean))
}

#' @export
natural2parameters.trunc_beta <- function(eta) {
	# eta: The natural parameters in a beta distribution
	# returns (alpha,beta)
	return(c(shape1 = eta[1], shape2 = eta[2]))
}

#' @title Convert parameters to Natural parameters
#' @param parms The shape1 and shape2 parameters in a beta distribution
#' @return The natural parameters
#' @author René Holst
#' @examples
#' sample.lognorm <- rtrunc(n=100000, meanlog=2.5, sdlog=0.5, a=7, family="log-normal")
#' ml_lognormal <- ml.estimation.trunc.dist(
#'   sample.lognorm, y.min = 7, max.it = 500, tol = 1e-10, delta = 0.3
#' )
#' eta.hat <- parameters2natural.trunc_beta(ml_lognormal)
#' @export
parameters2natural.trunc_beta <- function(parms) {
	# parms: The parameters shape and rate in a beta distribution
	# returns the natural parameters
	return(c(shape1 = parms[1], shape2 = parms[2]))
}

get.y.seq.trunc_beta <- function(y, y.min = 0, y.max=1, n = 100) {
	# needs chekking
	mean <- mean(y, na.rm = T)
	sd <- var(y, na.rm = T)^0.5
	lo <- max(y.min, mean - 5 * sd)
	hi <- min(y.max, mean + 5 * sd)
	return(seq(lo, hi, length = n))
}

get.grad.E.T.inv.trunc_beta <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta' : p x p matrix
  term.1=sum(1/(((1:10000)+eta[1]))^2)
  term.2=sum(1/(((1:10000)+eta[2]))^2)
  term.12=sum(1/(((1:10000)+eta[1]+eta[2]))^2)
  return(A = solve(matrix(c(term.1-term.12,-term.12 ,-term.12, term.2-term.12, ncol = 2))))
}
