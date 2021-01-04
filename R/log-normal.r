## --##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Log Normal distribution   ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##

sufficient.T.lognorm <- function(y) {
	return(suff.T = cbind(log(y), log(y)^2))
}

average.T.lognorm <- function(y) {
	return(apply(sufficient.T.lognorm(y), 2, mean))
}

#' @title Random Truncated Log-Normal
#' @param n sample size
#' @param mu mean of un-truncated distribution
#' @param sigma standard deviation of un-truncated distribution
#' @param a point of left truncation
#' @param b point of right truncation
#' @return A sample of size n drawn from a truncated log-normal distribution
#' @note The effective sample size is reduced due to truncation.
#' @author René Holst
#' @examples
#' sample.lognorm <- rtrunc.lognorm(n = 100000, mu = 2.5, sigma = 0.5, a = 7)
#' hist(
#'   sample.lognorm, nclass = 35, xlim = c(0, 60), freq = FALSE,
#'    ylim = c(0, 0.15)
#' )
#' @export
rtrunc.lognorm <- function(n, mu, sigma, a, b) {
	y <- rlnorm(n, mu, sigma)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	}
	return(y)
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
