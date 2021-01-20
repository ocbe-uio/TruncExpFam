#' @title The Truncated Exponential Family
#' @description Random generation for the truncated exponential family distributions.
#' @param n sample size
#' @param a point of left truncation
#' @param b point of right truncation
#' @param trials number of trials
#' @param prob probability of success on each trial
#' @param alpha shape of "parent" distribution
#' @param beta rate of "parent" distribution
#' @param mu mean of un-truncated distribution
#' @return A sample of size n drawn from a truncated distribution
#' @note The effective sample size is reduced due to truncation.
#' @author René Holst, Waldir Leôncio
#' @importFrom methods new
#' @examples
#' x <- rtrunc(n=1000, prob=0.6, trials=20, a=4, b=10) # Binomial
#' x # whole object
#' sample.binom <- x@sample # sample (probably smaller than 15 due to a and b)
#' plot(table(sample.binom), ylab="Frequency", main="Freq. of sampled values")
#' @export
# TODO: replace example with get/set functions
setGeneric(
	name = "rtrunc",
	def  = function(
		n, a, b,
		trials, prob,
		alpha, beta,
		mu
	) standardGeneric("rtrunc")
)

#' @title Method containing the parameters for the truncated binomial distribution
#' @inherit rtrunc
setMethod(
	f = "rtrunc",
	signature(
		n      = "numeric",
		a      = "numeric",
		b      = "numeric",
		trials = "numeric",
		prob   = "numeric",
		alpha   = "missing",
		beta   = "missing",
		mu     = "missing"
	),
	definition = function(n, a, b, trials, prob) {
		y <- rbinom(n, trials, prob)
		if (!missing(a)) {
			y <- y[y >= a]
		}
		if (!missing(b)) {
			y <- y[y <= b]
		}
		y <- new(
			"rtrunc-binomial", n=as.integer(n), a=a, b=b, sample=as.integer(y),
			trials=trials, prob=prob
		)
		return(y)
	}
)

#' @title Method containing the parameters for the truncated gamma distribution
#' @inherit rtrunc
setMethod(
	f = "rtrunc",
	signature(
		n      = "numeric",
		a      = "numeric",
		b      = "ANY",
		alpha  = "numeric",
		beta   = "numeric",
		trials = "missing",
		prob   = "missing",
		mu     = "missing"
	),
	definition = function(n, a, b, alpha, beta) {
		y <- rgamma(n, shape = alpha, rate = beta)
		if (!missing(a)) {
			y <- y[y >= a]
		}
		if (!missing(b)) {
			y <- y[y <= b]
		} else {
			b <- Inf
		}
		y <- new("rtrunc-gamma", n=as.integer(n), a=a, b=b, sample=y,
			alpha=alpha, beta=beta
		)
		return(y)
	}
)

#' @title Random Truncated Log-Normal
#' @param n sample size
#' @param mulog mean of un-truncated distribution
#' @param sigmalog standard deviation of un-truncated distribution
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
rtrunc.lognorm <- function(n, mulog, sigmalog, a, b) {
	y <- rlnorm(n, mulog, sigmalog)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	}
	return(y)
}

#' @title Random Truncated Normal
#' @param n sample size
#' @param mu mean of "parent" distribution
#' @param sigma standard deviation of "parent" distribution
#' @param a point of left truncation
#' @param b point of right truncation
#' @return A sample of size n drawn from a truncated normal distribution
#' @note The effective sample size is reduced due to truncation.
#' @author René Holst
#' @examples
#' sample.norm <- rtrunc.norm(n=10000,mu=2,sigma=1.5,a=-1)
#' hist(sample.norm, nclass = 25)
#' @export
rtrunc.norm <- function(n, mu, sigma, a, b) {
	y <- rnorm(n, mu, sigma)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	}
	return(y)
}

#' @title Random Truncated Poisson
#' @param n sample size
#' @param lambda mean and var of "parent" distribution
#' @param a point of left truncation
#' @param b point of right truncation
#' @return A sample of size n drawn from a truncated Poisson distribution
#' @note The effective sample size is reduced due to truncation. a, and b are included in the domain
#' @author René Holst
#' @examples
#' sample.pois <- rtrunc.pois(1000, 10, 4)
#' hist(sample.pois)
#' @export
rtrunc.pois <- function(n, lambda, a, b) {
	y <- rpois(n, lambda)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	}
	return(y)
}