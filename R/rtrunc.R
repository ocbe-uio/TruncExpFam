#' @title The Truncated Exponential Family
#' @description Random generation for the truncated exponential family distributions. Please ferer to the "Details" and "Examples" section for more information on how to use this function.
#' @param n sample size
#' @param a point of left truncation
#' @param b point of right truncation
#' @param family distribution family to use
#' @param ... individual arguments to each distribution
#' @return A sample of size n drawn from a truncated distribution
#' @note The effective sample size is reduced due to truncation.
#' @author René Holst, Waldir Leôncio
#' @details The best way to use this function is by calling the `rtrunc` generic with the `family` parameter of your choice. You can also specifically call one of the methods (e.g. `rtrunc.poisson(10, lambda=3)` instead of `rtrunc(10, family="poisson", lambda=3)), but the latter is more flexible (i.e., easily programmable) and more robust (i.e., it contains better error handling and validation procedures).
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
		"binomial", "gamma", "log-gamma", "loggamma", "log-normal", "lognormal",
		"gaussian", "normal", "poisson", "contbernoulli", "chisq"
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
	trunc_class <- genRtruncClass(n, family, names(list(...)))
	class(n) <- trunc_class
	rtrunc.generic(n, ...)
}

rtrunc.generic <- function(n, ...) {
	UseMethod("rtrunc", n)
}

# ======================================================== #
# Wrappers for rtrunc methods                              #
# ======================================================== #
# rtruncbeta <- rtrunc.beta # TODO: uncomment (#30)
#' @rdname rtrunc
#' @export
rtruncbinom <- rtrunc.binomial

#' @rdname rtrunc
#' @export
rtruncchisq <- rtrunc.chisq

#' @rdname rtrunc
#' @export
rtrunccontbernoulli <- rtrunc.contbernoulli

# rtruncexp <- rtrunc.exponential # TODO: uncomment (#30)

#' @rdname rtrunc
#' @export
rtruncgamma <- rtrunc.gamma

# rtruncinvgamma <- rtrunc.invgamma # TODO: uncomment (#30)

# rtruncinvnormal <- rtrunc.invnormal # TODO: uncomment (#30)

#' @rdname rtrunc
#' @export
rtrunclnorm <- rtrunc.lognormal

# rtruncnbinom <- rtrunc.nbinom # TODO: uncomment (#30)

#' @rdname rtrunc
#' @export
rtruncnorm <- rtrunc.normal

#' @rdname rtrunc
#' @export
rtruncpois <- rtrunc.poisson