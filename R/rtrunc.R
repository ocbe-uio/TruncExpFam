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
rtrunc <- function(n, family="gaussian", ...) {
	# ======================================================== #
	# Validating                                               #
	# ======================================================== #
	family <- tolower(family)
	valid_distros <- c(
		"binomial", "gamma", "log-gamma", "log-normal", "gaussian", "poisson", "contbernoulli", "chisq"
	)
	if (!(family %in% valid_distros)) {
		stop(
			"Invalid distribution family. Please choose from the list below:\n",
			paste(valid_distros, collapse=", ")
		)
	}

	# ======================================================== #
	# Dispatching functions                                    #
	# ======================================================== #
	# TODO: Replace if-despatching with 1) create new class 2) regular dispatching (inspiration: https://stackoverflow.com/a/66025891/1169233)
	# TODO: add parameter cheking here?
	if (family == "binomial") {
		rtrunc.binomial(n, ...)
	} else if (family == "gamma") {
		rtrunc.gamma(n, ...)
	} else if (family == "log-normal") {
		rtrunc.lognormal(n, ...)
	} else if (family == "contbernoulli") {
		rtrunc.contbernoulli(n, ...)
	} else if (family == "chisq") {
		rtrunc.chisq(n, ...)
	} else if (family == "poisson") {
		rtrunc.poisson(n, ...)
	} else if (family == "gaussian") {
		rtrunc.normal(n, ...)
	} else {
		stop("rtrunc method for family=", family, " not yet implemented.")
	}
}

#' @title Random Truncated Binomial
#' @param size number of size
#' @param prob probability of success on each trial
rtrunc.binomial <- function(n, size, prob, a, b) {
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

#' @title Random Truncated Gamma
#' @rdname rtrunc
#' @param shape shape of "parent" distribution
#' @param rate rate of "parent" distribution
rtrunc.gamma <- function(n, shape, rate, a=0, b=Inf) {
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
	class(y) <- "rtrunc-lognormal"
	return(y)
}

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
	class(y) <- "rtrunc-normal"
	return(y)
}

#' @title Random Truncated Poisson
#' @rdname rtrunc
#' @param lambda mean and var of "parent" distribution
rtrunc.poisson <- function(n, lambda, a, b) {
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


#' @title Random Truncated Continuous Bernoulli
#' @rdname rtrunc
#' @param lambda mean of "parent" distribution
rtrunc.contbernoulli <- function(n, lambda, a, b) {
	# Sampling function for a continuous bernoulli distribution
	# This distribution is not implemented in Base R
	# Used in the sampling of the truncated continuous bernoulli
	rcontbernoulli <- function(n, lambda){
		if ((lambda < 0) | (lambda > 1)) {
			stop("lambda must be in (0, 1)")
		}
		# TODO: issue a warning similar to the result from the call >rbinom(10,3,-0.1)
		u <- runif(n)
		if (lambda == 0.5) {
			return(u)
		}
		x <- log(1 + (2 * lambda - 1) * u / (1 - lambda)) / (log(lambda / (1 - lambda))) # The inverse of the CDF for a cont. bernoulli distribution
		class(x) <- "rtrunc-contbernoulli"
		return(x)
	}
	y <- rcontbernoulli(n, lambda)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	} else {
		b <- 1
	}
	class(y) <- "rtrunc-contbernoulli"
	return(y)
}

#' @title Random Truncated ChiSquare
#' @rdname rtrunc
#' @param df degrees of freedom for "parent" distribution
#' @importFrom stats dchisq pchisq rchisq
rtrunc.chisq <- function(n, df, a, b) {
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