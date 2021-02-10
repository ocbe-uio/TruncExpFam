## --##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Log Normal distribution   ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##

#' @title Random Truncated Log-Normal
#' @rdname rtrunc
#' @param meanlog mean of un-truncated distribution
#' @param sdlog standard deviation of un-truncated distribution
rtrunc.lognormal <- function(n, meanlog, sdlog, a, b) {
	y <- rlnorm(n, meanlog, sdlog)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	} else {
		b <- Inf
	}
	class(y) <- "trunc_lognormal"
	return(y)
}

sufficient.T.lognorm <- function(y) {
	return(suff.T = cbind(log(y), log(y)^2))
}

average.T.lognorm <- function(y) {
	return(apply(sufficient.T.lognorm(y), 2, mean))
}

dtrunc.trunc_lognormal <- function(y, eta, a = -Inf, b = Inf) {
	parm <- natural2parameters.norm(eta)
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

init.parms.lognorm <- function(y) {
	# Y~LN(mean,sigma) => X=log(Y)~N(mean,sigma)
	# Returns empirical parameter estimates for mean and sd
	# browser()
	x <- log(y)
	parm <- c(mean= mean(x), sd = sqrt(var(x)))
}

get.y.seq.lognorm <- function(y, y.min, y.max, n = 100) {
	x <- log(y)
	mean <- mean(x, na.rm = T)
	sd <- var(x, na.rm = T)^0.5
	lo <- max(y.min, exp(mean - 3.5 * sd))
	hi <- min(y.max, exp(mean + 3.5 * sd))
	return(seq(lo, hi, length = n))
}
