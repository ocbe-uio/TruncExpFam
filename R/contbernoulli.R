## --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the continuous Bernoulli distribution  ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

#' @importFrom stats runif
rtrunc.contbernoulli <- function(n, lambda, a, b = 1) {
	# Sampling function for a continuous bernoulli distribution
	# This distribution is not implemented in Base R
	# Used in the sampling of the truncated continuous bernoulli
	rcontbernoulli <- function(n, lambda){
		if ((lambda < 0) | (lambda > 1)) {
			stop("lambda must be in (0, 1)")
		}
		u <- runif(n)
		if (lambda == 0.5) {
			return(u)
		}
		x <- log(1 + (2 * lambda - 1) * u / (1 - lambda)) / (log(lambda / (1 - lambda))) # The inverse of the CDF for a cont. bernoulli distribution
		return(x)
	}
	y <- rcontbernoulli(n, lambda)
	if (!missing(a)) {
		y <- y[y >= a]
	}
	if (!missing(b)) {
		y <- y[y <= b]
	}
	class(y) <- "trunc_contbern"
	return(y)
}

# The two functions 'dcontbern' and 'pcontbern' below act in support of the
# truncated continuous bernoulli distribution, as base R does not include
# this distribution
# dcontbern is the untruncated function (which is not present in base R)
dcontbern <- function(x,lambda){
  if ((x<0)|(x>1))
    return(0)
  if (lambda==0.5)
    norm.const=2
  else
    norm.const=2*(atanh(1-2*lambda))/(1-2*lambda)
  d=norm.const*(lambda^x)*(1-lambda)^(1-x)
  return(d)
}

# untruncated version (not implemented in base R)
pcontbern <- function(x,lambda){
  if (x<0) p <- 0
  else if (x>1) p <- 1
  else if (lambda==0.5) p <- x
  else p <- ((lambda^x)*(1-lambda)^(1-x)+lambda-1)/(2*lambda-1)
  return(p)
}

#' @export
dtrunc.trunc_contbern <- function(y, eta, a = 0, b) {
	lambda <- natural2parameters.trunc_contbern(eta)
	dens <- ifelse((y <= a) | (y > b), 0, dcontbern(y, lambda=lambda))
	if (!missing(a)) {
		F.a <- pcontbern(a, lambda)
	} else {
		F.a <- 0
	}
	if (!missing(b)) {
		F.b <- pcontbern(b, lambda)
	} else {
		F.b <- 1
	}
	return(dens / (F.b - F.a))
}

#' @export
init.parms.trunc_contbern <- function(y) {
	# Returns empirical parameter estimate for the lambda parameter
	# Note: lambda cannot be expressed in closed form as a function of the mean
	parms <- mean(y)
	class(parms) <- "trunc_contbern"
	return(parms)
}

sufficient.T.trunc_contbern <- function(y) {
	return(suff.T = y)
}

average.T.trunc_contbern <- function(y) {
	return(mean(y))
}

#' @export
natural2parameters.trunc_contbern <- function(eta) {
	# eta: The natural parameters in a continuous bernoulli distribution distribution
	# returns rate
	return(c(lamda = 1/(1+exp(-eta))))
}

#' @export
parameters2natural.trunc_contbern <- function(parms) {
	# parms: The parameter lambda in a continuous bernoulli distribution
	# returns the natural parameters
	return(eta = log(parms/(1-parms)))
}

get.y.seq.trunc_contbern <- function(y, y.min = 0, y.max, n = 100) {
  mean <- mean(y, na.rm = T)
  var.y <- var(y, na.rm = T)
  lo <- max(round(y.min), 0)
  hi <- min(y.max, round(mean + 10 * sqrt(var.y)),1)
  return(	return(seq(lo, hi, length = n))
  )
}

get.grad.E.T.inv.trunc_contbern <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta
  exp.eta=exp(eta)
	return(A = ((exp.eta-1)*eta)^2/(exp.eta*(exp.eta-eta^2+eta-1)))
}

