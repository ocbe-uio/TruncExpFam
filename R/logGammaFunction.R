## --##--##--##--##--##--##--##--##--##--##--##--##--
# Evaluating the derivative of log(Gamma(x))
# See https://www.nbi.dk/~polesen/borel/node5.html
# sq=1:10000000
# C=sum(1/sq)-log(max(sq)) # 0.57721566490153286060
# C:  Euler-Mascheroni Constant.
psi <- function(x, k = 10000) {
	# OBS: This function is not used!
	-0.57721566490153286060 + sum(1 / (1:k) - 1 / ((1:k) + x - 1))
}

dpsi.dx <- function(x, k = 10000) {
	# Returns the derivative of the psi function above
	sum((1 / ((0:k) + x))^2)
}
