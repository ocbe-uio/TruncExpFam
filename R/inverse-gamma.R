## --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the inverse gamma distribution     ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

#' @export
#' @importFrom invgamma dinvgamma pinvgamma
dtrunc.trunc_invgamma <- function(y, eta, a, b) {
	# TODO: develop rtrunc.invgamma (#30)
	parm <- natural2parameters.trunc_invgamma(eta)
	dens <- ifelse((y < a) | (y > b), 0, dinvgamma(y, shape = parm[1], rate = parm[2]))
	if (!missing(a)) {
		F.a <- pinvgamma(a, shape = parm[1], rate = parm[2])
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

init.parms.trunc_invgamma <- function(y) {
	# Returns  parameter estimates mean and sd
	amean <- mean(y)
	avar <- var(y)
	alpha <- amean^2/avar+2
	beta  <- (alpha-1)*amean
	parms <- c(shape = alpha, rate = beta)
	class(parms) <- "trunc_invgamma"
	return(parms)
}

sufficient.T.trunc_invgamma <- function(y) {
	return(suff.T = cbind(log(y), 1/y))
}

average.T.trunc_invgamma <- function(y) {
	return(apply(cbind(log(y), 1/y), 2, mean))
}

#' @export
natural2parameters.trunc_invgamma <- function(eta) {
	# eta: The natural parameters in a inverse gamma distribution
	# returns (shape,rate)
	return(c(shape = -eta[1]-1, rate =-eta[2]))
}

#' @export
parameters2natural.trunc_invgamma <- function(parms) {
	# parms: The parameters shape and rate in a beta distribution
	# returns the natural parameters
	return(c(shape = -parms[1]-1, rate = -parms[2]))
}

get.y.seq.trunc_invgamma <- function(y, y.min = 1e-10, y.max=1, n = 100) {
	# needs chekking
	mean <- mean(y, na.rm = T)
	sd <- var(y, na.rm = T)^0.5
	lo <- max(y.min, mean - 5 * sd,1e-10)
	hi <- min(y.max, mean + 5 * sd)
	return(seq(lo, hi, length = n))
}

get.grad.E.T.inv.trunc_invgamma <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta' : p x p matrix
  A.11=sum(1/(((0:10000)+eta[1]+1))^2)
	return(A = solve(matrix(c(A.11,-1/eta[2] ,-1/eta[2], (eta[1]+1)/eta[2]^2, ncol = 2))))
}
