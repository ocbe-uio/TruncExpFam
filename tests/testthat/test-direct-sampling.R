context("Sampling directly from the truncated distibution")

test_that("Original parameters are retrieved", {
  norm_1 <- rtrunc(1e6, mean = 1, sd = 2, faster = TRUE)
  expect_equal(
    mlEstimationTruncDist(norm_1),
    c("mean" = 1, "sd" = 2),
    tolerance = 1e-2
  )
})
