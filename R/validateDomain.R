validateDomain <- function(n, ...) {
	parms <- list(...)
	UseMethod("validateDomain")
}

validateDomain.beta <- function(n, shape1, shape2, ...) {
	if (shape1 < 0) stop("Invalid domain. shapes1 must be non-negative.")
	if (shape2 < 0) stop("Invalid domain. shapes2 must be non-negative.")
}

validateDomain.binomial <- function(n, size, prob, ...) {
	if (size != as.integer(size) | size < 0) {
		stop("Invalid domain. size must be a natural number.")
	}
	if (prob < 0 | prob > 1) {
		stop("Invalid domain. prob must be [0, 1].")
	}
}

validateDomain.chisq <- function(n, df, ...) {
	if (df != as.integer(df) | df < 0) {
		stop("Invalid domain. df must be a natural positive number.")
	}
}

validateDomain.contbernoulli <- function(n, lambda, ...) {
	if (lambda <= 0 | lambda >= 1) {
		stop("Invalid domain. lambda must be (0, 1).")
	}
}

validateDomain.exp <- function(n, rate, ...) {
	if (rate <= 0) stop("Invalid domain. rate must be positive.")
}

validateDomain.gamma <- function(n, shape, rate, ...) {
	if (shape <= 0) stop("Invalid domain. shape must be > 0.")
	if (rate <= 0) stop("Invalid domain. rate must be > 0.")
}

validateDomain.invgamma <- function(n, shape, rate, ...) {
	if (shape <= 0) stop("Invalid domain. shape must be > 0.")
	if (rate <= 0) stop("Invalid domain. rate must be > 0.")
}

validateDomain.invgauss <- function(n, m, s, ...) {
	if (m <= 0) stop("Invalid domain. m must be > 0.")
	if (s <= 0) stop("Invalid domain. s must be > 0.")
}

validateDomain.lognormal <- function(n, meanlog, sdlog, ...) {
	if (is.complex(meanlog)) stop("Invalid domain. meanlog must be real.")
	if (sdlog <= 0) stop("Invalid domain. rate must be > 0.")
}

validateDomain.nbinom <- function(n, size, prob, mu, ...) {
	if (size != as.integer(size) | size < 0) {
		stop("Invalid domain. size must be a natural number.")
	}
	if (prob < 0 | prob > 1) {
		stop("Invalid domain. prob must be [0, 1].")
	}
	if (is.complex(mu)) stop("Invalid domain. mean must be real.")
}

validateDomain.normal <- function(n, mean, sd, ...) {
	if (is.complex(mean)) stop("Invalid domain. mean must be real.")
	if (sd <= 0) stop("Invalid domain. sd must be > 0.")
}

validateDomain.poisson <- function(n, lambda, ...) {
	if (lambda < 0) {
		stop("Invalid domain. lambda must be non-negative.")
	}
}
