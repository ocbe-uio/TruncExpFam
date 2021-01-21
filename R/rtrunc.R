#' @title The Truncated Exponential Family
#' @description Random generation for the truncated exponential family distributions.
#' @param n sample size
#' @param a point of left truncation
#' @param b point of right truncation
#' @param trials number of trials
#' @param prob probability of success on each trial
#' @param alpha shape of "parent" distribution
#' @param beta rate of "parent" distribution
#' @param mulog mean of un-truncated distribution
#' @param sigmalog standard deviation of un-truncated distribution
#' @param mu mean of parent distribution
#' @param sigma standard deviation is parent distribution
#' @param lambda mean and var of "parent" distribution
#' @return A sample of size n drawn from a truncated distribution
#' @note The effective sample size is reduced due to truncation.
#' @author René Holst, Waldir Leôncio
#' @importFrom methods new
#' @examples
#' # Truncated binomial distribution
#' x <- rtrunc(n=1000, prob=0.6, trials=20, a=4, b=10)
#' str(x) # whole object
#' sample.binom <- x@sample # sample (probably smaller than 15 due to a and b)
#' plot(table(sample.binom), ylab="Frequency", main="Freq. of sampled values")
#'
#' # Truncated Log-Normal distribution
#' sample.lognorm <- rtrunc(n=100000, mulog=2.5, sigmalog=0.5, a=7)@sample
#'
#' hist(
#'   sample.lognorm, nclass = 35, xlim = c(0, 60), freq = FALSE,
#'    ylim = c(0, 0.15)
#' )
#'
#' # Normal distribution
#' sample.norm <- rtrunc(n=10000,mu=2,sigma=1.5,a=-1)@sample
#' hist(sample.norm, nclass = 25)
#'
#' # Gamma distribution
#' sample.gamma <- rtrunc.gamma(n = 10000, alpha = 6, beta = 2, a = 2)@sample
#' hist(sample.gamma, nclass = 15)
#'
#' # Poisson distribution
#' sample.pois <- rtrunc(n=1000, lambda=10, a=4)@sample
#' hist(sample.pois)
#' @export

# TODO: replace "@" in examples with get/set functions

setGeneric(
	name = "rtrunc",
	def  = function(
		n, a, b,
		trials, prob,
		alpha, beta,
		mulog, sigmalog,
		mu, sigma,
		lambda
	) standardGeneric("rtrunc")
)

#' @title Random Truncated Binomial
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
		mulog = "missing",
		sigmalog = "missing",
		mu     = "missing",
		sigma  = "missing",
		lambda = "missing"
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

#' @title Random Truncated Gamma
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
		mulog = "missing",
		sigmalog = "missing",
		mu     = "missing",
		sigma  = "missing",
		lambda = "missing"
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
#' @inherit rtrunc
setMethod(
	f = "rtrunc",
	signature(
		n      = "numeric",
		a      = "numeric",
		b      = "ANY",
		trials = "missing",
		prob   = "missing",
		alpha  = "missing",
		beta   = "missing",
		mulog  = "numeric",
		sigmalog = "numeric",
		mu     = "missing",
		sigma  = "missing",
		lambda = "missing"
	),
	definition = function(n, a, b, mulog, sigmalog) {
		y <- rlnorm(n, mulog, sigmalog)
		if (!missing(a)) {
			y <- y[y >= a]
		}
		if (!missing(b)) {
			y <- y[y <= b]
		} else {
			b <- Inf
		}
		y <- new("rtrunc-lognorm", n=as.integer(n), a=a, b=b, sample=y,
			mulog=mulog, sigmalog=sigmalog
		)
		return(y)
	}
)

#' @title Random Truncated Normal
#' @inherit rtrunc
setMethod(
	f = "rtrunc",
	signature(
		n      = "numeric",
		a      = "numeric",
		b      = "ANY",
		trials = "missing",
		prob   = "missing",
		alpha  = "missing",
		beta   = "missing",
		mulog  = "missing",
		sigmalog = "missing",
		mu     = "numeric",
		sigma  = "numeric",
		lambda = "missing"
	),
	definition = function(n, a, b, mu, sigma) {
		y <- rnorm(n, mu, sigma)
		if (!missing(a)) {
			y <- y[y >= a]
		}
		if (!missing(b)) {
			y <- y[y <= b]
		} else {
			b <- Inf
		}
		y <- new("rtrunc-norm", n=as.integer(n), a=a, b=b, sample=y,
			mu=mu, sigma=sigma
		)
		return(y)
	}
)

#' @title Random Truncated Poisson
#' @inherit rtrunc
setMethod(
	f = "rtrunc",
	signature(
		n      = "numeric",
		a      = "numeric",
		b      = "ANY",
		trials = "missing",
		prob   = "missing",
		alpha  = "missing",
		beta   = "missing",
		mulog  = "missing",
		sigmalog = "missing",
		mu     = "missing",
		sigma  = "missing",
		lambda = "numeric"
	),
	definition = function(n, a, b, lambda) {
		y <- rpois(n, lambda)
		if (!missing(a)) {
			y <- y[y >= a]
		}
		if (!missing(b)) {
			y <- y[y <= b]
		} else {
			b <- Inf
		}
		y <- new(
			"rtrunc-pois", n=as.integer(n), a=a, b=b, sample=y,
			lambda=lambda
		)
		return(y)
	}
)