## --##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the gamma distribution     ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##

rtrunc.gamma <- function(n, shape, rate=1, a=0, b=Inf) {
	y <- rgamma(n, shape = shape, rate = rate)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	}
	class(y) <- "trunc_gamma"
	return(y)
}

#' @export
dtrunc.trunc_gamma <- function(y, eta, a, b) {
	parm <- natural2parameters.trunc_gamma(eta)
	dens <- ifelse((y < a) | (y > b), 0, dgamma(y, shape = parm[1], rate = parm[2]))
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

#' @export
init.parms.trunc_gamma <- function(y) {
	# Returns  parameter estimates mean and sd
	amean <- mean(y)
	avar <- var(y)
	shp <- amean^2 / avar
	parms <- c(shape = shp, rate = shp / amean)
	class(parms) <- "trunc_gamma"
	return(parms)
}

sufficient.T.trunc_gamma <- function(y) {
	return(suff.T = cbind(log(y), y))
}

average.T.trunc_gamma <- function(y) {
	return(apply(cbind(log(y), y), 2, mean))
}

#' @export
natural2parameters.trunc_gamma <- function(eta) {
	# eta: The natural parameters in a gamma distribution
	# returns (shape,rate)
	return(c(shape = eta[1] + 1, rate = -eta[2]))
}

#' @export
parameters2natural.trunc_gamma <- function(parms) {
	# parms: The parameters shape and rate in a gamma distribution
	# returns the natural parameters
  return(c(eta.1 = parms[1] - 1, eta.2 = -parms[2]))
}

get.y.seq.trunc_gamma <- function(y, y.min = 1e-6, y.max, n = 100) {
	# Bør chekkes
	mean <- mean(y, na.rm = T)
	sd <- var(y, na.rm = T)^0.5
	lo <- max(y.min, mean - 5 * sd)
	hi <- min(y.max, mean + 5 * sd)
	return(seq(lo, hi, length = n))
}

get.grad.E.T.inv.trunc_gamma <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta' : p x p matrix
	return(A = solve(matrix(c(-1 / eta[1]^2 + dpsi.dx(eta[1]), -1 / eta[2], -1 / eta[2], (eta[1] + 1) / eta[2]^2), ncol = 2)))
}
