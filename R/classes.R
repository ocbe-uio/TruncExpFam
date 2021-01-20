# ======================================================== #
# Defining classes                                         #
# ======================================================== #

setClass(
	Class = "rtrunc",
	slots = list(
		n = "integer",
		a = "numeric",
		b = "numeric",
		sample = "numeric"
	)
)
setClass(
	Class = "rtrunc-binomial",
	contains = "rtrunc",
	slots = list(
		trials = "numeric",
		prob   = "numeric"
	)
)
setClass(
	Class = "rtrunc-gamma",
	contains = "rtrunc",
	slots = list(
		alpha = "numeric",
		beta  = "numeric"
	)
)
setClass(
	Class = "rtrunc-lognorm",
	contains = "rtrunc",
	slots = list(
		mulog = "numeric",
		sigmalog = "numeric"
	)
)

# ======================================================== #
# Validating                                               #
# ======================================================== #

# setValidity(
# 	Class = "rtrunc",
# 	method = function(n, a, b, r) {
# 		if (n > 0) {
# 			TRUE
# 		} else {
# 			stop("n must be greater than zero.")
# 		}
# 	}
# )
# TODO: use setValidity() to control domain of classes
