# ======================================================== #
# Defining classes                                         #
# ======================================================== #

setClass(
	Class = "Trunc",
	slots = list(
		n = "integer",
		a = "numeric",
		b = "numeric",
		sample = "numeric"
	)
)
setClass(
	Class = "Truncated Binomial",
	contains = "Trunc",
	slots = list(
		trials = "numeric",
		prob   = "numeric"
	)
)
setClass(
	Class = "Truncated Gamma",
	contains = "Trunc",
	slots = list(
		alpha = "numeric",
		beta  = "numeric"
	)
)
setClass(
	Class = "Truncated Lognormal",
	contains = "Trunc",
	slots = list(
		mulog = "numeric",
		sigmalog = "numeric"
	)
)
setClass(
	Class = "Truncated Normal",
	contains = "Trunc",
	slots = list(
		mu = "numeric",
		sigma = "numeric"
	)
)
setClass(
	Class = "Truncated Poisson",
	contains = "Trunc",
	slots = list(
		lambda = "numeric"
	)
)

# ======================================================== #
# Validating                                               #
# ======================================================== #

# setValidity(
# 	Class = "Trunc",
# 	method = function(n, a, b, r) {
# 		if (n > 0) {
# 			TRUE
# 		} else {
# 			stop("n must be greater than zero.")
# 		}
# 	}
# )
# TODO: use setValidity() to control domain of classes
