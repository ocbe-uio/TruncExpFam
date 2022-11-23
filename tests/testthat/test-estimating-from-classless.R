context("Estimating from numeric (classless) vectors")

set.seed(2828324)

x <- rnorm(1e4, mean = 9, sd = 2)
y <- rbeta(1e4, shape1 = 9, shape2 = 2)
# TODO: add more as needed to test all distros

test_that("Estimation works, in general", {
  expect_error(mlEstimationTruncDist(x), "choose an underlying distribution")
  expect_named(mlEstimationTruncDist(x, family = "normal"), c("mean", "sd"))
  expect_named(mlEstimationTruncDist(x, family = "gamma"), c("shape", "rate"))
  expect_error(mlEstimationTruncDist(x, family = "beta"), "outside of support")
})

test_that("Original parameters are retrieved", {
  expect_equal(
    unclass(mlEstimationTruncDist(x, family = "normal")),
    c("mean" = 9, "sd" = 2),
    tol = 1e-1
  )
  expect_equal(
    unclass(mlEstimationTruncDist(x, family = "poisson")),
    c("lambda" = 9),
    tol = 1e-1
  )
})
