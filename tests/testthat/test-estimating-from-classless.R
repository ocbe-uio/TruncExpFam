context("Estimating from numeric (classless) vectors")

set.seed(2828324)

cont01 <- rbeta(1e4, shape1 = 10, shape2 = 100)
cont01b <- cont01 / max(cont01)  # better resembles a contbern candidate
disc0inf <- rpois(1e4, lambda = 10)
cont0inf <- rchisq(1e4, df = 10)
cont0infb <- exp(-cont0inf)
contReal <- rnorm(1e4, mean = 10, sd = 10)

test_that("Estimation works, in general", {
  # Improper samples
  expect_error(mlEstimationTruncDist(contReal), "choose an underlying distribution")
  expect_error(mlEstimationTruncDist(contReal, family = "beta"), "outside of support")

  # Proper samples
  expect_named(mlEstimationTruncDist(cont01, family = "beta"), c("shape1", "shape2"))
  expect_named(mlEstimationTruncDist(disc0inf, family = "binomial"), c("prob"))
  expect_named(mlEstimationTruncDist(cont0inf, family = "chisq"), c("df"))
  expect_named(mlEstimationTruncDist(cont01b, family = "contbern"), c("lambda"))
  expect_named(mlEstimationTruncDist(cont0infb, family = "exp"), c("rate"))
  expect_named(mlEstimationTruncDist(cont0inf, family = "gamma"), c("shape", "rate"))
  expect_named(mlEstimationTruncDist(cont0inf, family = "invgamma"), c("shape", "rate"))
  expect_named(mlEstimationTruncDist(cont0inf, family = "invgauss", tol = .1), c("m", "s"))
  expect_named(mlEstimationTruncDist(cont0inf, family = "lognormal"), c("mean", "sd"))
  expect_named(mlEstimationTruncDist(disc0inf, family = "nbinom"), c("mean"))
  expect_named(mlEstimationTruncDist(contReal, family = "normal"), c("mean", "sd"))
  expect_named(mlEstimationTruncDist(disc0inf, family = "poisson"), c("lambda"))
})

test_that("Original parameters are retrieved", {
  expect_equal(
    mlEstimationTruncDist(contReal, family = "normal"),
    c("mean" = 10, "sd" = 10),
    tol = 1e-1, check.attributes = FALSE
  )
  expect_equal(
    mlEstimationTruncDist(disc0inf, family = "poisson"),
    c("lambda" = 10),
    tol = 1e-1, check.attributes = FALSE
  )
})
