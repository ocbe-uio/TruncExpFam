context("Sampling directly from the truncated distibution")

test_that("Original parameters are retrieved", {
  set.seed(2723347)
  norm_1 <- rtrunc(1e6, mean = 1, sd = 2, faster = TRUE)
  expect_equal(
    mlEstimationTruncDist(norm_1),
    c("mean" = 1, "sd" = 2),
    tolerance = 1e-2
  )
  beta_1 <- rtrunc(1e5, shape1 = 3, shape2 = 9, faster = TRUE, family = "beta")
  expect_equal(
    mlEstimationTruncDist(beta_1),
    c("shape1" = 3, "shape2" = 9),
    tolerance = 1e-2
  )
  chisq_1 <- rtrunc(1e5, df = 30, faster = TRUE, family = "chisq")
  expect_equal(
    mlEstimationTruncDist(chisq_1),
    c("df" = 30),
    tolerance= 1e-2
  )
})

test_that("Truncation is not a speed limiter", {})
