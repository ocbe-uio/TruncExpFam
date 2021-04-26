## --##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the normal distribution   ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##

#' @title Random Truncated Normal
#' @rdname rtrunc
#' @param mean mean of parent distribution
#' @param sd standard deviation is parent distribution
rtrunc.normal <- function(n, mean, sd, a, b) {
	y <- rnorm(n, mean, sd)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	} else {
		b <- Inf
	}
	class(y) <- "trunc_normal"
	return(y)
}

#' @export
dtrunc.trunc_normal <- function(y, eta, a = -Inf, b = Inf) {
	parm <- natural2parameters.trunc_normal(eta)
	dens <- ifelse((y < a) | (y > b), 0, dnorm(y, mean = parm[1], sd = parm[2]))
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

#' @export
init.parms.trunc_normal <- function(y) {
	# Returns empirical parameter estimates mean and sd
	parms <- c(mean = mean(y), sd = sqrt(var(y)))
	class(parms) <- "trunc_normal"
	return(parms)
}

sufficient.T.trunc_normal <- function(y) {
	return(suff.T = cbind(y, y^2))
}

average.T.trunc_normal <- function(y) {
	return(apply(sufficient.T.trunc_normal(y), 2, mean))
}

#' @export
natural2parameters.trunc_normal <- function(eta) {
	# eta: The natural parameters in a normal distribution
	# returns (mean,sigma)
	return(c(mean = -0.5 * eta[1] / eta[2], sd = sqrt(-0.5 / eta[2])))
}

#' @export
parameters2natural.trunc_normal <- function(parms) {
	# parms: The parameters mean and sd in a normal distribution
	# returns the natural parameters
	return(c(eta.1 = parms[1], eta.2 = -0.5) / parms[2]^2)
}

get.y.seq.trunc_normal <- function(y, y.min, y.max, n = 100) {
	mean <- mean(y, na.rm = T)
	sd <- var(y, na.rm = T)^0.5
	lo <- max(y.min, mean - 3.5 * sd)
	hi <- min(y.max, mean + 3.5 * sd)
	return(seq(lo, hi, length = n))
}

get.grad.E.T.inv.trunc_normal <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta' : p x p matrix
	return(A = solve(0.5 * matrix(c(-1 / eta[2], eta[1] / eta[2]^2, eta[1] / eta[2]^2, 1 / eta[2]^2 - eta[1]^2 / eta[2]^3), ncol = 2)))
}
