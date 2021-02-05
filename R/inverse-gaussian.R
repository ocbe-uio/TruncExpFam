## --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the inverse gaussian distribution  ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

#' @importFrom rmutil dinvgauss
density.trunc.invgauss <- function(y, eta, a = -Inf, b = Inf) {
	parm <- natural2parameters.invgauss(eta)
	dens <- ifelse((y < a) | (y > b), 0, dinvgauss(y, mean = parm[1], shape = parm[2]))
	if (!missing(a)) {
		F.a <- pnorm(a, parm[1], parm[2])
	} else {
		F.a <- 0
	}
	if (!missing(b)) {
		F.b <- pnorm(b, parm[1], parm[2])
	} else {
		F.b <- 1
	}
	const <- 1 / (F.b - F.a)
	return(dens / (F.b - F.a))
}

init.parms.invgauss <- function(y) {
	# Returns empirical parameter estimates mean and shape
  mean=mean(y)
  shp=1/(mean(1/y)-1/mean)
	return(c(mean = mean, shape = shp))
}

sufficient.T.invgauss <- function(y) {
	return(suff.T = cbind(y, 1/y))
}

average.T.invgauss <- function(y) {
	return(apply(sufficient.T.invgauss(y), 2, mean))
}

natural2parameters.invgauss <- function(eta) {
	# eta: The natural parameters in an inverse gaussian distribution
	# returns (mean,shape)
	return(c(mean = sqrt(eta[2] / eta[1]), shape = -2*eta[2]))
}

parameters2natural.invgauss <- function(parms) {
	# parms: The parameters mean and shape in a normal distribution
	# returns the natural parameters
	return(c(eta.1 = -parms[2]/(2*parms[1]^2), eta.2 = -0.5*parms[2]))
}

get.y.seq.invgauss <- function(y, y.min, y.max, n = 100) {
	mean <- mean(y, na.rm = T)
	shape <- var(y, na.rm = T)^0.5
	lo <- max(max(0,y.min), mean - 3.5 * sd) # FIXME: sd not defined
	hi <- min(y.max, mean + 3.5 * sd)
	return(seq(lo, hi, length = n))
}

get.grad.E.T.inv.invgauss <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta' : p x p matrix
  sqrt.eta1=sqrt(eta[1]); sqrt.eta2=sqrt(eta[2])
	return(A = solve(0.5 * matrix(c(-sqrt.eta2/sqrt.eta1^3, 1/(sqrt.eta1*sqrt.eta2), 1/(sqrt.eta1*sqrt.eta2),
	                                -sqrt.eta1/sqrt.eta2^3+1/sqrt.eta2^2), ncol = 2)))
}
