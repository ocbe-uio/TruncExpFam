#' @title Probability Density Function
#' @description Calculates the PDF for a given truncated distribution
#' @param y output from rtrunc
#' @param eta Natural parameters
#' @param a lower truncation limit
#' @param b upper truncation limit
#' @export
#' @examples
#' y <- rtrunc(50, mean=5, sd=2)
#' dtrunc(y, eta=c(0, -1))
#'
dtrunc <- function(y, eta, a, b) {
	UseMethod("dtrunc", y)
}

#' @title Initialize parameters
#' @description Returns the empirical parameter estimate for a distribution
#' @param y output of rtrunc
#' @examples
#' # Normal distribution
#' sampNorm <- rtrunc(50, mean=5, sd=2)
#' TruncExpFam:::init.parms(sampNorm)
#'
#' # Poisson distribution
#' sampPois <- rtrunc(10, lambda=100, family="Poisson")
#' TruncExpFam:::init.parms(sampPois)
init.parms <- function(y) {
	UseMethod("init.parms")
}

#' @title Convert natural parameters to distribution parameters
#' @param eta vector of natural parameters
#' @return Distribution parameters
#' @export
#' @examples
#' samp <- rtrunc(n=100, lambda=2, family="Poisson")
#' TruncExpFam:::natural2parameters(TruncExpFam:::init.parms(samp))
#'
# ASK: check if natural2parameters should also be called directly from
# the output of ml.estimation.trunc.dist()
# ex.: ml <- ml.estimation.trunc.dist(samp); parameters2natural.trunc_gamma(ml)
natural2parameters <- function(eta) {
	UseMethod("natural2parameters")
}

#' @title Convert distribution parameters to natural parameters
#' @param parms A vector of parameters in a distribution distribution
#' @return The natural parameters
#' @examples
#' # Poisson distribution
#' samp <- rtrunc(n=100, lambda=2, family="Poisson")
#' TruncExpFam:::parameters2natural(TruncExpFam:::init.parms(samp))
#'
# ASK: check if natural2parameters should also be called directly from
# the output of ml.estimation.trunc.dist()
# ex.: ml <- ml.estimation.trunc.dist(samp); parameters2natural.trunc_gamma(ml)
parameters2natural <- function(parms) {
	UseMethod("parameters2natural")
}
