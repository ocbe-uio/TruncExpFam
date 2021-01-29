#' @title The Truncated Exponential Family
#' @description Random generation for the truncated exponential family distributions.
#' @param n sample size
#' @param a point of left truncation
#' @param b point of right truncation
#' @return A sample of size n drawn from a truncated distribution
#' @note The effective sample size is reduced due to truncation.
#' @author René Holst
#' @importFrom methods new
#' @examples
#' # Truncated binomial distribution
#' sample.binom <- rtrunc(n=1000, prob=0.6, size=20, a=4, b=10)
#' sample.binom
#' plot(table(sample.binom), ylab="Frequency", main="Freq. of sampled values")
#'
#' # Truncated Log-Normal distribution
#' sample.lognorm <- rtrunc(n=100000, mulog=2.5, sigmalog=0.5, a=7)
#' summary(sample.lognorm)
#'
#' hist(
#'   sample.lognorm, nclass = 35, xlim = c(0, 60), freq = FALSE,
#'    ylim = c(0, 0.15)
#' )
#'
#' # Normal distribution
#' sample.norm <- rtrunc(n=10000,mu=2,sigma=1.5,a=-1)
#' head(sample.norm)
#' hist(sample.norm, nclass = 25)
#'
#' # Gamma distribution
#' sample.gamma <- rtrunc(n = 10000, alpha = 6, beta = 2, a = 2)
#' hist(sample.gamma, nclass = 15)
#'
#' # Poisson distribution
#' sample.pois <- rtrunc(n=1000, lambda=10, a=4)
#' sample.pois
#' plot(table(sample.pois))
#' @export

# TODO: replace "@" in examples with get/set functions

setGeneric(
	name = "rtrunc",
	def  = function(
		n,
		size, prob,
		alpha, beta,
		mulog, sigmalog,
		mu, sigma,
		lambda,
		df,
		a, b
	) standardGeneric("rtrunc"),
	signature = c("prob", "alpha", "mulog", "mu", "lambda","df")
)

#' @title Random Truncated Binomial
#' @param size number of size
#' @param prob probability of success on each trial
#' @rdname rtrunc
setMethod(
	f = "rtrunc",
	signature(
		prob = "numeric",
		alpha   = "missing",
		mulog = "missing",
		mu     = "missing",
		lambda = "missing",
		df     = "missing"
	),
	definition = function(n, size, prob, a, b) {
		y <- rbinom(n, size, prob)
		if (!missing(a)) {
			y <- y[y >= a]
		}
		if (!missing(b)) {
			y <- y[y <= b]
		}
		class(y) <- "rtrunc-binomial"
		return(y)
	}
)

#' @title Random Truncated Gamma
#' @rdname rtrunc
#' @param alpha shape of "parent" distribution
#' @param beta rate of "parent" distribution
setMethod(
	f = "rtrunc",
	signature(
		prob = "missing",
		alpha  = "numeric",
		mulog = "missing",
		mu     = "missing",
		lambda = "missing",
		df     = "missing"
	),
	definition = function(n, alpha, beta, a=0, b=Inf) {
		y <- rgamma(n, shape = alpha, rate = beta)
		if (!missing(a)) {
			y <- y[y >= a]
		}
		if (!missing(b)) {
			y <- y[y <= b]
		} else {
			b <- Inf
		}
		class(y) <- "rtrunc-gamma"
		return(y)
	}
)

#' @title Random Truncated Log-Normal
#' @rdname rtrunc
#' @param mulog mean of un-truncated distribution
#' @param sigmalog standard deviation of un-truncated distribution
setMethod(
	f = "rtrunc",
	signature(
		prob = "missing",
		alpha  = "missing",
		mulog  = "numeric",
		mu     = "missing",
		lambda = "missing",
		df     = "missing"
  ),
	definition = function(n, mulog, sigmalog, a, b) {
		y <- rlnorm(n, mulog, sigmalog)
		if (!missing(a)) {
			y <- y[y >= a]
		}
		if (!missing(b)) {
			y <- y[y <= b]
		} else {
			b <- Inf
		}
		class(y) <- "rtrunc-lognormal"
		return(y)
	}
)

#' @title Random Truncated Normal
#' @rdname rtrunc
#' @param mu mean of parent distribution
#' @param sigma standard deviation is parent distribution
setMethod(
	f = "rtrunc",
	signature(
		prob = "missing",
		alpha  = "missing",
		mulog  = "missing",
		mu     = "numeric",
		lambda = "missing",
		df     = "missing"
	),
	definition = function(n, mu, sigma, a, b) {
		y <- rnorm(n, mu, sigma)
		if (!missing(a)) {
			y <- y[y >= a]
		}
		if (!missing(b)) {
			y <- y[y <= b]
		} else {
			b <- Inf
		}
		class(y) <- "rtrunc-normal"
		return(y)
	}
)

#' @title Random Truncated Poisson
#' @rdname rtrunc
#' @param lambda mean and var of "parent" distribution
setMethod(
	f = "rtrunc",
	signature(
		prob = "missing",
		alpha  = "missing",
		mulog  = "missing",
		mu     = "missing",
		lambda = "numeric",
		df     = "missing"
	),
	definition = function(n, lambda, a, b) {
		y <- rpois(n, lambda)
		if (!missing(a)) {
			y <- y[y >= a]
		}
		if (!missing(b)) {
			y <- y[y <= b]
		} else {
			b <- Inf
		}
		class(y) <- "rtrunc-poisson"
		return(y)
	}
)

#' @title Random Truncated Bernoulli
#' @rdname rtrunc
#' @param prob mean of "parent" distribution
setMethod(
  f = "rtrunc",
  signature(
    prob = "numeric",
    alpha  = "missing",
    mulog  = "missing",
    mu     = "missing",
    lambda = "missing",
    df     = "missing"
  ),
  definition = function(n, prob, a, b) {
    y <- rbinom(n, 1, prob)
    if (!missing(a)) {
      y <- y[y >= a]
    }
    if (!missing(b)) {
      y <- y[y <= b]
    } else {
      b <- 1
    }
    class(y) <- "rtrunc-bernoulli"
    return(y)
  }
)

#' @title Random Truncated ChiSquare
#' @rdname rtrunc
#' @param df degrees of freedom for "parent" distribution
setMethod(
  f = "rtrunc",
  signature(
    prob = "missing",
    alpha  = "missing",
    mulog  = "missing",
    mu     = "missing",
    lambda = "missing",
    df=    "numeric"
  ),
  definition = function(n, df, a, b) {
    y <- rchisq(n, df)
    if (!missing(a)) {
      y <- y[y >= a]
    }
    if (!missing(b)) {
      y <- y[y <= b]
    } else {
      b <- Inf
    }
    class(y) <- "rtrunc-chisq"
    return(y)
  }
)