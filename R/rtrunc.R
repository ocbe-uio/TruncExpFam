#' @title The Truncated Exponential Family
#' @description Random generation for the truncated exponential family distributions.
#' @param n sample size
#' @param a point of left truncation
#' @param b point of right truncation
#' @param family distribution family to use
#' @param ... individual arguments to each distribution
#' @return A sample of size n drawn from a truncated distribution
#' @note The effective sample size is reduced due to truncation.
#' @author René Holst
#' @importFrom methods new
#' @examples
#' # Truncated binomial distribution
#' sample.binom <- rtrunc(1000, family="binomial", prob=0.6, size=20, a=4, b=10)
#' sample.binom
#' plot(table(sample.binom), ylab="Frequency", main="Freq. of sampled values")
#'
#' # Truncated Log-Normal distribution
#' sample.lognorm <- rtrunc(
#'   n=100000, family="log-normal", meanlog=2.5, sdlog=0.5, a=7
#' )
#' summary(sample.lognorm)
#'
#' hist(
#'   sample.lognorm, nclass = 35, xlim = c(0, 60), freq = FALSE,
#'   ylim = c(0, 0.15)
#' )
#'
#' # Normal distribution
#' sample.norm <- rtrunc(n=10000, mean=2, sd=1.5, a=-1)
#' head(sample.norm)
#' hist(sample.norm, nclass = 25)
#'
#' # Gamma distribution
#' sample.gamma <- rtrunc(n=10000, family="gamma", shape=6, rate=2, a=2)
#' hist(sample.gamma, nclass = 15)
#'
#' # Poisson distribution
#' sample.pois <- rtrunc(n=1000, family="poisson", lambda=10, a=4)
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
		message("Sampling from the truncated binomial distribution")
		rtrunc.binomial(n, ...)
	} else if (family == "gamma") {
		message("Sampling from the truncated gamma distribution")
		rtrunc.gamma(n, ...)
	} else if (family == "log-normal") {
		message("Sampling from the truncated log-normal distribution")
		rtrunc.lognormal(n, ...)
	} else if (family == "contbernoulli") {
		message("Sampling from the truncated continuous Bernoulli distribution")
		rtrunc.contbernoulli(n, ...)
	} else if (family == "chisq") {
		message("Sampling from the truncated chi-squared distribution")
		rtrunc.chisq(n, ...)
	} else if (family == "poisson") {
		message("Sampling from the truncated Poisson distribution")
		rtrunc.poisson(n, ...)
	} else if (family %in% c("gaussian", "normal")) {
		message("Sampling from the truncated normal distribution")
		rtrunc.normal(n, ...)
	} else {
		stop("rtrunc method for family=", family, " not yet implemented.")
	}
}
