context("Sampling directly from the truncated distibution")

test_that("Original parameters are retrieved", {
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
})
