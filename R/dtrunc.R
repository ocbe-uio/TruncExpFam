#' @title Probability Density Function
#' @description Calculates the PDF for a given truncated distribution
#' @note Either the common or the natural parameters must be provided.
#' @param y output from rtrunc or any valid numeric value(s).
#' @export
#' @return The density of `y` for the given values of the `eta` parameter.
#' @examples
#' # Using the output of rtrunc
#' y <- rtrunc(50, mean = 5, sd = 2)
#' dtrunc(y, eta = c(0, -1))
#'
#' # Directly-inputting values
#' dtruncnorm(y = c(5, 0, -10), eta = c(0, -0.05))
dtrunc <- function(y, ...) {
  UseMethod("dtrunc", y)
}
