# ======================================================== #
# Sampling                                                 #
# ======================================================== #

context("Sampling with rtrunc")

set.seed(117)
sample.norm <- rtrunc(n = 10000, mean= 2, sd= 1.5, a = -1, family="gaussian")
set.seed(117)
sample.lognorm <- rtrunc(
	n = 100000, meanlog = 2.5, sdlog = 0.5, a = 7, family="log-normal"
)
set.seed(117)
sample.pois <- rtrunc(n=1000, lambda=10, a=4, family="poisson")
set.seed(117)
sample.binom <- rtrunc(n=1000, prob=0.6, size=20, a=4, b=10, family="binomial")
set.seed(117)
sample.gamma <- rtrunc(n = 10000, shape = 6, rate = 2, a = 2, family="gamma")

# TODO: export density.* functions as S3 methods
# y <- seq(-3, 60, length = 200)
# lines(y, density.trunc.lognorm(y, eta = c(10, -2), a = 7), lwd = 2, col = 2)
# hist(log(sample.lognorm))

test_that("rtrunc samples have the expected values", {
	tol <- 1e-3
	expect_equal(head(sample.norm, 3), c(2.8645444, 2.1329365, 3.0388173))
	expect_equal(head(sample.lognorm, 3), c(16.2514, 12.7345, 17.2235), tol=tol)
	expect_equal(head(sample.pois), c(11, 10, 12, 18, 12, 11))
	expect_equal(head(sample.binom), c(11, 16, 12, 12, 10, 14))
	expect_equal(head(sample.gamma, 3), c(3.4673, 2.8549, 3.6220), tol = 1e-4)
})

test_that("Output of rtrunc matches stats::r*", {
	expect_setequal(
		object   = {set.seed(1); rnorm(500, mean=1, sd=3)},
		expected = {set.seed(1); rtrunc(500, mean=1, sd=3)}
	)
	expect_setequal(
		object   = {set.seed(1); rbinom(50, size=10, prob=.3)},
		expected = {set.seed(1); rtrunc(50, size=10, prob=.3, family="binomial")}
	)
	expect_setequal(
		object   = {set.seed(1); rgamma(50, shape=4, rate=5)},
		expected = {set.seed(1); rtrunc(50, shape=4, rate=5, family="gamma")}
	)
	expect_setequal(
		object   = {set.seed(1); rlnorm(50, meanlog=7, sdlog=2)},
		expected = {set.seed(1); rtrunc(50, meanlog=7, sdlog=2, family="log-normal")}
	)
	expect_setequal(
		object   = {set.seed(1); rpois(500, lambda=72)},
		expected = {set.seed(1); rtrunc(500, lambda=72, family="poisson")}
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
# FIXME: ml.estimation.trunc.dist not working for binomial (#19)
# ml_binom <- ml.estimation.trunc.dist(
# 	sample.binom, y.min = 4, max.it = 500, delta = 0.33, family = "Binomial",
# 	nsize = 10, print.iter = FALSE
# )
ml_gamma <- ml.estimation.trunc.dist(
	sample.gamma, y.min = 2, max.it = 1500, delta = 0.3, family = "Gamma",
	print.iter = FALSE
)

test_that("ml.estimation.trunc.dist works", {
	expect_equal(ml_gaussian, c(mean= 2, sd = 1.5), tol = 1e-1)
	expect_equal(ml_lognormal, c(mean= 2.5, sd = 0.5), tol = 1e-1)
	expect_equal(ml_poisson, c(lambda = 10), tol = 1e-1)
	# TODO: add unit test for ml_binom (depends on #19)
	expect_equal(ml_gamma, c(shape = 6, rate = 2), tol = 1e-1)
})

# ======================================================== #
# Parameter conversion                                     #
# ======================================================== #

context("Parameter conversion")

eta.hat <- parameters2natural.gamma(ml_lognormal)

test_that("Converting parameters", {
	expect_equal(eta.hat, c(eta.1.mean= 1.5207, eta.2.sd = -0.4842), tol = 1e-3)
})
