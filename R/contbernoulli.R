## --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the continuous Bernoulli distribution  ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##


# The two functions 'dcontbern' and 'pcontbern' below act in support of the
# truncated continuous bernoulli distribution, as base R does not include
# this distribution
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

pcontbern <- function(x,lambda){
  if (x<0) p <- 0
  else if (x>1) p <- 1
  else if (lambda==0.5) p <- x
  else p <- ((lambda^x)*(1-lambda)^(1-x)+lambda-1)/(2*lambda-1)
  return(p)
}


density.trunc.contbern <- function(y, eta, a = 0, b) {
	lambda <- natural2parameters.contbern(eta)
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

init.parms.contbern <- function(y) {
	# Returns empirical parameter estimate for the lambda parameter
  # Note: lambda cannot be expressed in closed form as a function of the mean
	return(mean(y))
}

sufficient.T.contbern <- function(y) {
	return(suff.T = y)
}

average.T.contbern <- function(y) {
	return(mean(y))
}

natural2parameters.contbern <- function(eta) {
	# eta: The natural parameters in a continuous bernoulli distribution distribution
	# returns rate
	return(c(lamda = 1/(1+exp(-eta))))
}

parameters2natural.contbern <- function(parms) {
	# parms: The parameter lambda in a continuous bernoulli distribution
	# returns the natural parameters
	return(eta = log(parms/(1-parms)))
}

get.y.seq.contbern <- function(y, y.min = 0, y.max, n = 100) {
  mean <- mean(y, na.rm = T)
  var.y <- var(y, na.rm = T)
  lo <- max(round(y.min), 0)
  hi <- min(y.max, round(mean + 10 * sqrt(var.y)),1)
  return(	return(seq(lo, hi, length = n))
  )
}

get.grad.E.T.inv.contbern <- function(eta) {
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta
  exp.eta=exp(eta)
	return(A = ((exp.eta-1)*eta)^2/(exp.eta*(exp.eta-eta^2+eta-1)))
}

