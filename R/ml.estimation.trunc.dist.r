#' @title ML Estimation of Distribution Parameters
#' @description ML-estimation of the parameters of the distribution of the specified family, truncated at y.min and y.max
#' @param y Sequence spanning the domain of the truncated distribution
#' @param y.min Lower bound for y
#' @param y.max Upper bound for y
#' @param family Distribution family (Gaussian, LogNormal, Gamma, Poisson, Binomial)
#' @param tol Error tolerance for parameter estimation
#' @param delta Indirectly, the difference between consecutive iterations to compare with the error tolerance
#' @param max.it Maximum number of iterations
#' @param print.iter Print information about each iteration?
#' @param ... other parameters passed to the get.y.seq subfunctions
#' @note `print.iter` can be `TRUE`, `FALSE` or an integer indicating an interval for printing every `X` iterations.
#' @references Inspired by Salvador: Pueyo: "Algorithm for the maximum likelihood estimation of the parameters of the truncated normal and lognormal distributions"
#' @author René Holst
#' @importFrom stats dbinom dgamma dlnorm dnorm dpois pbinom pgamma plnorm pnorm ppois rbinom rgamma rlnorm rnorm rpois var
#' @examples
#' # Normal
#' sample.norm <- rtrunc(n = 10000, mu = 2, sigma = 1.5, a = -1)@sample
#' ml.estimation.trunc.dist(
#'   sample.norm, y.min = -1, max.it = 500, delta = 0.33, family = "Gaussian",
#'   print.iter = TRUE
#' )
#'
#' # Log-Normal
#' sample.lognorm <- rtrunc(n = 100000, mulog = 2.5, sigmalog = 0.5, a = 7)@sample
#' ml_lognormal <- ml.estimation.trunc.dist(
#'   sample.lognorm, y.min = 7, max.it = 500, tol = 1e-10, delta = 0.3,
#'   family = "LogNormal", print.iter = FALSE
#' )
#' ml_lognormal
#'
#' # Poisson
#' sample.pois <- rtrunc(n=1000, lambda=10, a=4)@sample
#' ml.estimation.trunc.dist(
#'   sample.pois, y.min = 4, max.it = 500, delta = 0.33, family = "Poisson",
#'   print.iter = 5
#' )
#'
#' # Gamma
#' sample.gamma <- rtrunc(n = 10000, alpha = 6, beta = 2, a = 2)@sample
#' ml.estimation.trunc.dist(
#'   sample.gamma, y.min = 2, max.it = 1500, delta = 0.3, family = "Gamma",
#'   print.iter = 10
#' )
#' @export
# TODO: add option to suppress output
ml.estimation.trunc.dist <- function(y, y.min = -Inf, y.max = Inf, family = "Gaussian", tol = 1e-5, max.it = 25, delta = 0.33, print.iter = TRUE, ...) {
	get.T.minus.E.T <- function(eta) {
		# Calculates T.bar-E(T|eta_j) by numerical integration
		delta.y <- y.seq[2] - y.seq[1] # step length, length(y.seq)=L
		trunc.density <- density.trunc(y.seq, eta, y.min, y.max) # L vector
		T.f <- sufficient.T(y.seq) * trunc.density # L x p matrix
		if (length(eta) > 1) {
			E.T.j <- delta.y * apply(T.f, 2, sum) # 1 x p
			if (cont.dist == T) {
				E.T.j <- E.T.j - delta.y * 0.5 * (T.f[1, ] + T.f[length(y.seq), ])
			}
		}
		else {
			E.T.j <- delta.y * sum(T.f)
			if (cont.dist == T) {
				E.T.j <- E.T.j - delta.y * 0.5 * (T.f[1] + T.f[length(y.seq)])
			}
		}
		T.bar.minus.E.T.j <- T.avg - E.T.j # 1 x p
		return(T.bar.minus.E.T.j)
	}

	# Some initialisations
	if (family == "Gaussian") {
		if (as.numeric(print.iter) > 0) message("Normal\n")
		init.parms <- init.parms.norm
		sufficient.T <- sufficient.T.norm
		average.T <- average.T.norm
		natural2parameters <- natural2parameters.norm
		parameters2natural <- parameters2natural.norm
		density.trunc <- density.trunc.norm
		get.grad.E.T.inv <- get.grad.E.T.inv.norm
		get.y.seq <- get.y.seq.norm
		cont.dist <- T
	}
	if (family == "LogNormal") {
		if (as.numeric(print.iter) > 0) message("Log Normal\n")
		init.parms <- init.parms.lognorm
		sufficient.T <- sufficient.T.lognorm
		average.T <- average.T.lognorm
		natural2parameters <- natural2parameters.norm
		parameters2natural <- parameters2natural.norm
		density.trunc <- density.trunc.lognorm
		get.grad.E.T.inv <- get.grad.E.T.inv.norm
		get.y.seq <- get.y.seq.lognorm
		cont.dist <- T
	}
	if (family == "Gamma") {
		if (as.numeric(print.iter) > 0) message("Gamma\n")
		init.parms <- init.parms.gamma
		sufficient.T <- sufficient.T.gamma
		average.T <- average.T.gamma
		natural2parameters <- natural2parameters.gamma
		parameters2natural <- parameters2natural.gamma
		density.trunc <- density.trunc.gamma
		get.grad.E.T.inv <- get.grad.E.T.inv.gamma
		get.y.seq <- get.y.seq.gamma
		cont.dist <- T
	}
	if (family == "Poisson") {
		if (as.numeric(print.iter) > 0) message("Poisson\n")
		init.parms <- init.parms.pois
		sufficient.T <- sufficient.T.pois
		average.T <- average.T.pois
		natural2parameters <- natural2parameters.pois
		parameters2natural <- parameters2natural.pois
		density.trunc <- density.trunc.pois
		get.grad.E.T.inv <- get.grad.E.T.inv.pois
		get.y.seq <- get.y.seq.pois
		cont.dist <- F
	}
	if (family == "Binomial") {
		if (as.numeric(print.iter) > 0) message("Binomial\n")
		init.parms <- init.parms.binomial
		sufficient.T <- sufficient.T.binomial
		average.T <- average.T.binomial
		natural2parameters <- natural2parameters.binomial
		parameters2natural <- parameters2natural.binomial
		density.trunc <- density.trunc.binomial
		get.grad.E.T.inv <- get.grad.E.T.inv.binomial
		get.y.seq <- get.y.seq.binomial
		cont.dist <- F
	}
	T.avg <- average.T(y)
	eta.j <- parameters2natural(init.parms(y))
	y.seq <- get.y.seq(y, y.min, y.max, n = 100, ...) # y-values to be used for calculation of the expectations
	it <- 0
	delta.L2 <- 10000
	# Now iterate
	while ((delta.L2 > tol) & (it < max.it)) {
		parm.j <- natural2parameters(eta.j)
		T.minus.E.T <- get.T.minus.E.T(eta.j)
		grad.E.T.inv <- get.grad.E.T.inv(eta.j) # p x p
		delta.eta.j.plus.1 <- delta * grad.E.T.inv %*% T.minus.E.T
		eta.j <- eta.j + delta.eta.j.plus.1
		delta.L2 <- sum(delta.eta.j.plus.1^2)
		it <- it + 1
		if (print.iter) {
			if (it %% as.numeric(print.iter) == 0) {
				cat(
					"it: ", it, "tol: ", delta.L2, " - parm: ",
					round(parm.j, 3), "\n"
				)
			}
		}
	}
	parm <- natural2parameters(eta.j)
	return(parm)
}
