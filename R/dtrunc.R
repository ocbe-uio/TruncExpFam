#' @title Probability Density Function
#' @description Calculates the PDF for a given truncated distribution
#' @param y output from rtrunc
#' @param ... arguments to be passed to the methods
#' @export
dtrunc <- function(y, ...) {
	UseMethods("dtrunc", y)
}