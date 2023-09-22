## --##--##--##--##--##--##--##--##--##--##--##--##--
# Evaluating the derivative of log(Gamma(x))
# See https://www.nbi.dk/~polesen/borel/node5.html
# C:  Euler-Mascheroni Constant.
dpsi.dx <- function(x, k = 10000) {
  # Returns the derivative of the psi function (removed from above)
  sum((1 / ((0:k) + x))^2)
}
