#' @title The Truncated Exponential Family
#' @description Random generation for
#' @return
#' @export
setGeneric(
	name = "rtrunc",
	def  = function(
		n,
		trials, prob,
		alfa, beta,
		mu,
		a, b, ...) standardGeneric("rtrunc")
)

# n: Sample size
# prob: probability of success on each trial in the "parent" distribution
# a, b: points of left and right truncation
# # OBS: a, and b are included in the domain
# returns a sample of size n drawn from a truncated binomialson distribution
# Note the effective sample size is reduced due to truncation
#' @title Random Truncated Binomial
#' @param n sample size
#' @param trials number of trials
#' @param probs probability of success on each trial
#' @param a point of left truncation
#' @param b point of right truncation
#' @return A sample of size n drawn from a truncated binomial distribution
#' @note The effective sample size is reduced due to truncation.
#' @author René Holst, Waldir Leôncio
#' @examples
#' rtrunc(n=15L, prob=.4, a=5, b=10, trials=10)
setMethod(
	f         = "rtrunc",
	signature(
		n      = "numeric",
		trials = "numeric",
		prob   = "numeric",
		alfa   = "missing",
		beta   = "missing",
		a      = "numeric",
		b      = "numeric",
		mu     = "missing"
	),
	definition = function(n, trials, prob, a, b) {
		y <- rbinom(n, trials, prob)
		if (!missing(a)) {
			y <- y[y >= a]
		}
		if (!missing(b)) {
			y <- y[y <= b]
		}
		y <- new(
			"rtrunc-binomial", n=as.integer(n), a=a, b=b, r=as.integer(y),
			trials=trials, prob=prob
		)
		return(y)
	}
)

#' @title Random Truncated Gamma
#' @param n sample size
#' @param alpha shape of "parent" distribution
#' @param beta rate of "parent" distribution
#' @param a point of left truncation
#' @param b point of right truncation
#' @return A sample of size n drawn from a truncated gamma distribution
#' @note The effective sample size is reduced due to truncation.
#' @author René Holst
#' @examples
#' sample.gamma <- rtrunc.gamma(n = 10000, alpha = 6, beta = 2, a = 2)
#' hist(sample.gamma,nclass=15)
#' @export
rtrunc.gamma <- function(n, alpha, beta, a, b) {
	y <- rgamma(n, shape = alpha, rate = beta)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	}
	return(y)
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