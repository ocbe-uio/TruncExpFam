# ======================================================== #
# Sampling                                                 #
# ======================================================== #

context("Sampling with rtrunc")

set.seed(117)
sample.norm <- rtrunc(n = 10000, mean = 2, sd = 1.5, a = -1, family = "gaussian")
set.seed(117)
sample.lognorm <- rtrunc(
  n = 100000, meanlog = 2.5, sdlog = 0.5, a = 7, family = "lognormal"
)
set.seed(117)
sample.pois <- rtrunc(n = 1000, lambda = 10, a = 4, family = "poisson")
set.seed(117)
sample.binom <- rtrunc(n = 1000, prob = 0.6, size = 20, a = 4, b = 10, family = "binomial")
set.seed(117)
sample.gamma <- rtrunc(n = 10000, shape = 6, rate = 2, a = 2, family = "gamma")
sample.nbinom <- rtruncnbinom(10000, size = 50, prob = .3, a = 100, b = 120)
sample.contbern <- rtrunccontbern(100, lambda = .4, b = .5)
sample.beta <- rtruncbeta(1000, shape1 = 15, shape2 = 4, a = .7, b = .9)

test_that("rtrunc samples have the expected values", {
  tol <- 1e-3
  expect_equal(head(sample.norm, 3), c(2.8645444, 2.1329365, 3.0388173))
  expect_equal(head(sample.lognorm, 3), c(16.2514, 12.7345, 17.2235), tol = tol)
  expect_equal(head(sample.pois), c(11, 10, 12, 18, 12, 11))
  expect_equal(head(sample.binom), c(10, 10, 9, 10, 8, 9))
  expect_equal(head(sample.gamma, 3), c(3.4673, 2.8549, 3.6220), tol = 1e-4)
  expect_equal(head(sample.nbinom), c(114, 101, 110, 100, 118, 116))
  expect_equal(head(sample.contbern, 3), c(0.06070758, 0.30618084, 0.29766296))
  expect_equal(head(sample.beta, 3), c(0.88398, 0.88610, 0.80583), tol = 1e-4)
})

test_that("Truncation limits are observed", {
  expect_true(all(sample.norm >= -1))
  expect_true(all(sample.lognorm >= -1))
  expect_true(all(sample.pois >= 4))
  expect_true(all(sample.binom >= 4) & all(sample.binom <= 10))
  expect_true(all(sample.gamma >= 2))
  expect_true(all(sample.nbinom >= 100) & all(sample.nbinom <= 120))
  expect_true(all(sample.contbern <= .5))
  expect_true(all(sample.beta >= .7) & all(sample.beta <= .9))
})

# ======================================================== #
# ML estimation                                            #
# ======================================================== #

context("ML estimation")

ml_gaussian <- mlEstimationTruncDist(
  sample.norm,
  max.it = 500, delta = 0.33,
  print.iter = FALSE
)
ml_lognormal <- mlEstimationTruncDist(
  sample.lognorm,
  max.it = 500, tol = 1e-10, delta = 0.3,
  print.iter = FALSE
)
ml_poisson <- mlEstimationTruncDist(
  sample.pois,
  max.it = 500, delta = 0.33,
  print.iter = FALSE
)
ml_binom <- mlEstimationTruncDist(
  sample.binom,
  max.it = 500, delta = 0.33,
  print.iter = FALSE
)
ml_gamma <- mlEstimationTruncDist(
  sample.gamma,
  max.it = 1500, delta = 0.3,
  print.iter = FALSE
)
ml_nbinom <- mlEstimationTruncDist(
  sample.nbinom,
  max.it = 500, delta = 0.33,
  print.iter = FALSE,
  r = 10
)
ml_contbern <- mlEstimationTruncDist(
  sample.contbern, print.iter = FALSE, tol = 1e-7, max.it = 1e3
)
# ml_beta <- mlEstimationTruncDist(
#   sample.beta, print.iter = FALSE, tol = 1e-7, max.it = 1e3
# ) # FIXME #85: often doesn't converge

test_that("mlEstimationTruncDist works", {
  expect_equal(unclass(ml_gaussian), c(mean = 2, sd = 1.5), tol = 1e-1)
  expect_equal(unclass(ml_lognormal), c(mean = 2.5, sd = 0.5), tol = 1e-1)
  expect_equal(unclass(ml_poisson), c(lambda = 10), tol = 1e-1)
  expect_equal(unclass(ml_binom), c(prob = 0.6), tol = 1e-1)
  expect_equal(unclass(ml_gamma), c(shape = 6, rate = 2), tol = 1e-1)
  expect_equal(unclass(ml_nbinom), c(mean = 110.4), tol = 1e-1)
  expect_equal(unclass(ml_contbern), c(lambda = 0.4), tol = 1e-1)
  # expect_equal(unclass(ml_beta), c(shape1 = 1508, shape2 = 441), tol = 1e-1) # FIXME #85
})

# ======================================================== #
# Parameter conversion                                     #
# ======================================================== #

context("Parameter conversion")

eta.hat <- parameters2natural.trunc_gamma(ml_lognormal)

test_that("Converting parameters", {
  expect_equal(unclass(eta.hat), c(eta.1.mean = 1.5, eta.2.sd = -0.5), tol = 5e-1)
})
