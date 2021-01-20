## --##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Log Normal distribution   ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##

sufficient.T.lognorm <- function(y) {
	return(suff.T = cbind(log(y), log(y)^2))
}

average.T.lognorm <- function(y) {
	return(apply(sufficient.T.lognorm(y), 2, mean))
}

density.trunc.lognorm <- function(y, eta, a = -Inf, b = Inf) {
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
	# Y~LN(mu,sigma) => X=log(Y)~N(mu,sigma)
	# Returns empirical parameter estimates for mu and sd
	# browser()
	x <- log(y)
	parm <- c(mu = mean(x), sd = sqrt(var(x)))
}

get.y.seq.lognorm <- function(y, y.min, y.max, n = 100) {
	x <- log(y)
	mu <- mean(x, na.rm = T)
	sd <- var(x, na.rm = T)^0.5
	lo <- max(y.min, exp(mu - 3.5 * sd))
	hi <- min(y.max, exp(mu + 3.5 * sd))
	return(seq(lo, hi, length = n))
}
