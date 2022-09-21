#' @title Extract parameters
#' @param y Numeric vector containing observations from a random variable
#' @param family Distribution family to assume for \code{y}
#' @param natural Should output be in terms of the natural parameter eta?
#' @param ... arguments passed to [init.parms()]
#' @export
#' @examples
#' x <- rtrunc(n = 100, lambda = 2, family = "Poisson")
#' attributes(x) <- NULL
#' class(x)
#' extractParameters(x, family = "normal")
#' extractParameters(x, family = "normal", natural = TRUE)
#' extractParameters(x, family = "binomial", nsize = 3)
#' extractParameters(x, family = "poisson", natural = FALSE)
#' extractParameters(x, family = "poisson", natural = TRUE)
extractParameters <- function(y, family = "gaussian", natural = FALSE, ...) {
  class(y) <- paste0("trunc_", useStandardFamilyName(family))
  parms <- init.parms(y, ...)
  if (natural) parms <- parameters2natural(parms)
  return(parms)
}
