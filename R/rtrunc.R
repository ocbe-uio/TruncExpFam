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
#' sample.lognorm <- rtrunc(n=100000, meanlog=2.5, sdlog=0.5, a=7)
#' summary(sample.lognorm)
#'
#' hist(
#'   sample.lognorm, nclass = 35, xlim = c(0, 60), freq = FALSE,
#'    ylim = c(0, 0.15)
#' )
#'
#' # Normal distribution
#' sample.norm <- rtrunc(n=10000,mean=2,sd=1.5,a=-1)
#' head(sample.norm)
#' hist(sample.norm, nclass = 25)
#'
#' # Gamma distribution
#' sample.gamma <- rtrunc(n = 10000, shape = 6, rate = 2, a = 2)
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
		shape, rate,
		meanlog, sdlog,
		mean, sd,
		lambda,
		df,
		a, b
	) standardGeneric("rtrunc"),
	signature = c("prob", "size", "shape", "meanlog", "mean", "lambda","df")
)

# Sampling function for a continuous bernoulli distribution
# This distribution is not implemented in Base R
# Used in the sampling of the truncated continuous bernoulli
rcontbernoulli=function(n,lambda){
  if ((lambda<0)|(lambda>1))
    stop("lambda")
  # issue a warning similar to the result from the call >rbinom(10,3,-0.1)
  u=runif(n)
  if (lambda==0.5)
    return(u)
  x=log(1+(2*lambda-1)*u/(1-lambda))/(log(lambda/(1-lambda))) # The inverse of the CDF for a cont. bernoulli distribution
  return(x)
}


#' @title Random Truncated Binomial
#' @param size number of size
#' @param prob probability of success on each trial
#' @rdname rtrunc
setMethod(
	f = "rtrunc",
	signature(
		prob = "numeric",
		size = "numeric",
		shape   = "missing",
		meanlog = "missing",
		mean     = "missing",
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
#' @param shape shape of "parent" distribution
#' @param rate rate of "parent" distribution
setMethod(
	f = "rtrunc",
	signature(
		prob = "missing",
		size = "missing",
		shape  = "numeric",
		meanlog = "missing",
		mean   = "missing",
		lambda = "missing",
		df     = "missing"
	),
	definition = function(n, shape, rate, a=0, b=Inf) {
		y <- rgamma(n, shape = shape, rate = rate)
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
#' @param meanlog mean of un-truncated distribution
#' @param sdlog standard deviation of un-truncated distribution
setMethod(
	f = "rtrunc",
	signature(
		prob = "missing",
		size = "missing",
		shape  = "missing",
		meanlog  = "numeric",
		mean   = "missing",
		lambda = "missing",
		df     = "missing"
  ),
	definition = function(n, meanlog, sdlog, a, b) {
		y <- rlnorm(n, meanlog, sdlog)
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
#' @param mean mean of parent distribution
#' @param sd standard deviation is parent distribution
setMethod(
	f = "rtrunc",
	signature(
		prob = "missing",
		size = "missing",
		shape  = "missing",
		meanlog  = "missing",
		mean   = "numeric",
		lambda = "missing",
		df     = "missing"
	),
	definition = function(n, mean, sd, a, b) {
		y <- rnorm(n, mean, sd)
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
		size = "missing",
		shape  = "missing",
		meanlog  = "missing",
		mean     = "missing",
		lambda = "numeric",
		df     = "missing"
	),
	definition = function(n, lambda, a, b) {  # FIXME: same signature as ContBer
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


#' @title Random Truncated Continuous Bernoulli
#' @rdname rtrunc
#' @param lambda mean of "parent" distribution
setMethod(
  f = "rtrunc",
  signature(
    prob = "missing",
  	size = "missing",
    shape  = "missing",
    meanlog  = "missing",
    mean     = "missing",
    lambda = "numeric",
    df     = "missing"
  ),
  definition = function(n, lambda, a, b) { # FIXME: same signature as Poisson!
    y <- rcontbernoulli(n,lambda)
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
#' @importFrom stats dchisq pchisq rchisq
setMethod(
  f = "rtrunc",
  signature(
    prob = "missing",
	  size = "missing",
    shape  = "missing",
    meanlog  = "missing",
    mean     = "missing",
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