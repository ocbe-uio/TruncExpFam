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