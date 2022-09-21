#' @title Extract parameters
#' @param y Numeric vector containing observations from a random variable
#' @param family Distribution family to assume for \code{y}
#' @param natural Should output be in terms of the natural parameter eta?
#' @param ... arguments passed to [init.parms()]
#' @export
#' @examples
#' # Some random data
#' x <- c(
#'   4, 3, 6, 3, 3, 3, 3, 4, 3, 2, 3, 0, 4, 2, 0, 1, 4, 3, 0, 0, 2, 3, 0, 3, 7,
#'   2, 1, 1, 2, 3, 2, 3, 3, 3, 2, 2, 2, 0, 2, 0, 2, 1, 0, 2, 3, 1, 0, 4, 2, 2,
#'   0, 1, 1, 1, 2, 2, 3, 1, 3, 1, 1, 0, 3, 3, 2, 0, 2, 2, 3, 0, 2, 1, 0, 0, 1,
#'   0, 2, 4, 2, 3, 3, 0, 1, 0, 5, 2, 4, 2, 7, 4, 4, 1, 2, 4, 3, 2, 4, 3, 1, 3
#' )
#'
#' # Extracting parameters under different distribution assumptions
#' extractParameters(x, family = "normal")
#' extractParameters(x, family = "normal", natural = TRUE)
#' extractParameters(x, family = "binomial", nsize = 3)
#' extractParameters(x, family = "poisson", natural = FALSE)
#' extractParameters(x, family = "poisson", natural = TRUE)
extractParameters <- function(y, family = "gaussian", natural = FALSE, ...) {
  class(y) <- paste0("trunc_", useStandardFamilyName(family))
  parms <- init.parms(y, ...)
  validateSupport(y, as.list(parms), ...)
  if (natural) parms <- parameters2natural(parms)
  return(parms)
}
