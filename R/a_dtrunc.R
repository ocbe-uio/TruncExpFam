#' @title Probability Density Function
#' @description Calculates the PDF for a given truncated distribution
#' @param y output from rtrunc
#' @param eta Natural parameters
#' @param a lower truncation limit
#' @param b upper truncation limit
#' @export
#' @examples
#' y <- rtrunc(50, mean = 5, sd = 2)
#' dtrunc(y, eta = c(0, -1))
dtrunc <- function(y, eta, a, b) {
  UseMethod("dtrunc", y)
}
