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
#' # samp <- rtrunc(n=100, lambda=20, family="Poisson")
#' # TruncExpFam:::natural2parameters()
natural2parameters <- function(eta) {
	UseMethod("natural2parameters")
}

#' @title Convert distribution parameters to natural parameters
#' @param parms A vector of parameters in a distribution distribution
#' @return The natural parameters
#' @examples
#' # Beta distribution
#' # samp <- rtrunc(n=100000, meanlog=2.5, sdlog=0.5, a=7, family="log-normal")
#' # ml <- ml.estimation.trunc.dist(
#' #   samp, y.min = 7, max.it = 500, tol = 1e-10, delta = 0.3
#' # )
#' # eta.hat <- parameters2natural.trunc_beta(ml_lognormal)
#' #' sample.lognorm <- rtrunc(n = 100000, meanlog = 2.5, sdlog = 0.5, a = 7, family="log-normal")
#'
#' # Gamma distribution
#' # ml_lognormal <- ml.estimation.trunc.dist(
#' #  sample.lognorm, y.min = 7, max.it = 500, tol = 1e-10, delta = 0.3,
#' #)
#' # eta.hat <- parameters2natural.trunc_gamma(ml_lognormal)
parameters2natural <- function(parms) {
	UseMethod("parameters2natural")
}