context("Normal distribution")

set.seed(117)
sample.norm <- rtrunc(n = 10000, mu = 2, sigma = 1.5, a = -1)@sample
ml_gaussian <- ml.estimation.trunc.dist(
	sample.norm, y.min = -1, max.it = 500, delta = 0.33, family = "Gaussian"
)

test_that("rtrunc.norm works", {
	expect_equal(head(sample.norm, 3), c(2.8645444, 2.1329365, 3.0388173))
})

context("Log-normal distribution")

set.seed(117)
sample.lognorm <- rtrunc(n = 100000, mulog = 2.5, sigmalog = 0.5, a = 7)@sample

# TODO: export density.* functions as S3 methods
# y <- seq(-3, 60, length = 200)
# lines(y, density.trunc.lognorm(y, eta = c(10, -2), a = 7), lwd = 2, col = 2)
# hist(log(sample.lognorm))

ml_lognormal <- ml.estimation.trunc.dist(
	sample.lognorm, y.min = 7, max.it = 500, tol = 1e-10, delta = 0.3,
	family = "LogNormal"
)
eta.hat <- parameters2natural.gamma(ml_lognormal)

# lines(y, density.trunc.lognorm(y, eta = eta.hat, a = 7), lwd = 2, col = 4)

test_that("rtrunc.norm works", {
	expect_equal(eta.hat, c(eta.1.mu = 1.5207, eta.2.sd = -0.4842), tol = 1e-3)
})

context("Poisson distribution")

set.seed(117)
sample.pois <- rtrunc(n=1000, lambda=10, a=4)@sample
hist(sample.pois)
ml_poisson <- ml.estimation.trunc.dist(
	sample.pois, y.min = 4, max.it = 500, delta = 0.33, family = "Poisson"
)

test_that("rtrunc.pois works", {
	expect_equal(head(sample.pois), c(11, 10, 12, 18, 12, 11))
})

context("Binomial distribution")

set.seed(117)
# NOT WORKING YET
sample.binom <- rtrunc(n=1000, prob=0.6, trials=20, a=4, b=10)@sample
# ml.estimation.trunc.dist(sample.binom, y.min = 4, max.it = 500, delta = 0.33, family = "Binomial", nsize = 10)
# FIXME: ml.estimation.trunc.dist not working for binomial

context("Gamma distribution")

set.seed(117)
sample.gamma <- rtrunc(n = 10000, alpha = 6, beta = 2, a = 2)@sample

ml_gamma <- ml.estimation.trunc.dist(
	sample.gamma, y.min = 2, max.it = 1500, delta = 0.3, family = "Gamma"
)

test_that("rtrunc.gamma works", {
	expect_equal(head(sample.gamma, 3), c(3.4673, 2.8549, 3.6220), tol = 1e-4)
})

context("ML estimation")

test_that("ml.estimation.trunc.dist works", {
	expect_equal(ml_gaussian, c(mu = 2.041146, sd = 1.481114), tol = 1e-6)
	expect_equal(ml_lognormal, c(mu = 2.5207512, sd = 0.4842092), tol = 1e-6)
	expect_equivalent(ml_poisson, 10.18402, tol = 1e-5) # TODO: name output lambda
})


# TODO: add tests comparing rgamma with rtrunc.gamma (for same seed, same results) and other distros