## --##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Log Normal distribution   ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##

#' @param meanlog mean of un-truncated distribution
#' @param sdlog standard deviation of un-truncated distribution
#' @rdname rtrunc
#' @export
rtrunclnorm <- rtrunc.lognormal <- function(n, meanlog, sdlog, a = -Inf, b = Inf) {
	y <- rlnorm(n, meanlog, sdlog)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	}
	class(y) <- "trunc_lognormal"
	y <- attachDistroAttributes(y, gsub("trunc_", "", class(y)))
	return(y)
}

sufficientT.trunc_lognormal <- function(y) {
	return(suff.T = cbind(log(y), log(y)^2))
}

averageT.trunc_lognormal <- function(y) {
	return(apply(sufficientT.trunc_lognormal(y), 2, mean))
}

#' @export
dtrunc.trunc_lognormal <- function(y, eta, a = -Inf, b = Inf) {
	parm <- natural2parameters.trunc_normal(eta)
	dens <- ifelse((y < a) | (y > b), 0, dlnorm(y, meanlog = parm[1], sdlog = parm[2]))
	if (!missing(a)) {
		F.a <- plnorm(a, parm[1], parm[2])
	} else {
		F.a <- 0
	}
	if (!missing(b)) {
		F.b <- plnorm(b, parm[1], parm[2])
	} else {
		F.b <- 1
	}
	const <- 1 / (F.b - F.a)
	return(dens / (F.b - F.a))
}

#' @rdname dtrunc
#' @export
dtrunclnorm <- dtrunc.trunc_lognormal

#' @export
init.parms.trunc_lognormal <- function(y) {
	# Y~LN(mean,sigma) => X=log(Y)~N(mean,sigma)
	# Returns empirical parameter estimates for mean and sd
	# browser()
	x <- log(y)
	parms <- c(mean= mean(x), sd = sqrt(var(x)))
	class(parms) <- "trunc_lognormal"
	return(parms)
}

getYseq.trunc_lognormal <- function(y, y.min, y.max, n = 100) {
	x <- log(y)
	mean <- mean(x, na.rm = T)
	sd <- var(x, na.rm = T)^0.5
	lo <- max(y.min, exp(mean - 3.5 * sd))
	hi <- min(y.max, exp(mean + 3.5 * sd))
	out <- seq(lo, hi, length = n)
	class(out) <- class(y)
	return(out)
}

# TODO #63: properly write the functions below
natural2parameters.trunc_lognormal <- function(eta) {
	warning("Using natural2parameters() from the Normal distribution")
	natural2parameters.trunc_normal(eta)
}

parameters2natural.trunc_lognormal <- function(parms) {
	warning("Using parameters2natural() from the Normal distribution")
	parameters2natural.trunc_normal(parms)
}

getGradETinv.trunc_lognormal <- function(eta) {
	warning("Using getGradETinv() from the Normal distribution")
	getGradETinv.trunc_normal(eta)
}
