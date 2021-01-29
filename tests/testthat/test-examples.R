# ======================================================== #
# Sampling                                                 #
# ======================================================== #

context("Sampling with rtrunc")

set.seed(117)
sample.norm <- rtrunc(n = 10000, mu = 2, sigma = 1.5, a = -1)
set.seed(117)
sample.lognorm <- rtrunc(n = 100000, mulog = 2.5, sigmalog = 0.5, a = 7)
set.seed(117)
sample.pois <- rtrunc(n=1000, lambda=10, a=4)
set.seed(117)
sample.binom <- rtrunc(n=1000, prob=0.6, size=20, a=4, b=10)
set.seed(117)
sample.gamma <- rtrunc(n = 10000, alpha = 6, beta = 2, a = 2)

# TODO: export density.* functions as S3 methods
# y <- seq(-3, 60, length = 200)
# lines(y, density.trunc.lognorm(y, eta = c(10, -2), a = 7), lwd = 2, col = 2)
# hist(log(sample.lognorm))

test_that("rtrunc.norm works", {
	expect_equal(head(sample.norm, 3), c(2.8645444, 2.1329365, 3.0388173))
})
test_that("rtrunc.pois works", {
	expect_equal(head(sample.pois), c(11, 10, 12, 18, 12, 11))
})
test_that("rtrunc.gamma works", {
	expect_equal(head(sample.gamma, 3), c(3.4673, 2.8549, 3.6220), tol = 1e-4)
})

context("Matching output of stats::r*")

test_that("stats::rnorm", {
	expect_setequal(
		object   = {set.seed(1); rnorm(500, mean=1, sd=3)},
		expected = {set.seed(1); rtrunc(500, mu=1, sigma=3)}
	)
})
test_that("stats::rbinom", {
	expect_setequal(
		object   = {set.seed(1); rbinom(500, size=10, prob=.3)},
		expected = {set.seed(1); rtrunc(500, size=10, prob=.3)}
	)
})
test_that("stats::rgamma", {
	expect_setequal(
		object   = {set.seed(1); rgamma(500, shape=4, rate=5)},
		expected = {set.seed(1); rtrunc(500, alpha=4, beta=5)}
	)
})
test_that("stats::rlnorm", {
	expect_setequal(
		object   = {set.seed(1); rlnorm(500, meanlog=7, sdlog=2)},
		expected = {set.seed(1); rtrunc(500, mulog=7, sigmalog=2)}
	)
})
test_that("stats::rpois", {
	expect_setequal(
		object   = {set.seed(1); rpois(500, lambda=72)},
		expected = {set.seed(1); rtrunc(500, lambda=72)}
	)
})

# ======================================================== #
# Matching output                                          #
# ======================================================== #

context("Matching output of stats::r*")

# ======================================================== #
# ML estimation                                            #
# ======================================================== #

context("ML estimation")

ml_gaussian <- ml.estimation.trunc.dist(
	sample.norm, y.min = -1, max.it = 500, delta = 0.33, family = "Gaussian",
	print.iter = FALSE
)
ml_lognormal <- ml.estimation.trunc.dist(
	sample.lognorm, y.min = 7, max.it = 500, tol = 1e-10, delta = 0.3,
	family = "LogNormal", print.iter = FALSE
)
ml_poisson <- ml.estimation.trunc.dist(
	sample.pois, y.min = 4, max.it = 500, delta = 0.33, family = "Poisson",
	print.iter = FALSE
)
# FIXME: ml.estimation.trunc.dist not working for binomial
# ml_binom <- ml.estimation.trunc.dist(
# 	sample.binom, y.min = 4, max.it = 500, delta = 0.33, family = "Binomial",
# 	nsize = 10, print.iter = FALSE
# )
ml_gamma <- ml.estimation.trunc.dist(
	sample.gamma, y.min = 2, max.it = 1500, delta = 0.3, family = "Gamma",
	print.iter = FALSE
)

test_that("ML estimation for binomial works", {

})
test_that("ml.estimation.trunc.dist works", {
	expect_equal(ml_gaussian, c(mu = 2.041146, sd = 1.481114), tol = 1e-6)
	expect_equal(ml_lognormal, c(mu = 2.5207512, sd = 0.4842092), tol = 1e-6)
	expect_equivalent(ml_poisson, 10.18402, tol = 1e-5) # TODO: name output lambda
})

# ======================================================== #
# Parameter conversion                                     #
# ======================================================== #

eta.hat <- parameters2natural.gamma(ml_lognormal)

test_that("Converting parameters", {
	expect_equal(eta.hat, c(eta.1.mu = 1.5207, eta.2.sd = -0.4842), tol = 1e-3)
})
