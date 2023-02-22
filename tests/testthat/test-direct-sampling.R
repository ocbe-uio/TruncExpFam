context("Sampling directly from the truncated distibution")

test_that("Original attributes are retrieved", {
  set.seed(2723347)

  # Normal
  norm_1 <- rtrunc(1e6, mean = 1, sd = 2, faster = TRUE)
  expect_equal(
    mlEstimationTruncDist(norm_1),
    c("mean" = 1, "sd" = 2),
    tolerance = 1e-2
  )
  expect_equal(
    attributes(norm_1),
    list(
      "class" = "trunc_normal",
      "parameters" = list("mean" = 1, "sd" = 2),
      "truncation_limits" = list("a" = -Inf, "b" = Inf),
      "continuous" = TRUE
    )
  )

  # Beta
  beta_1 <- rtrunc(1e5, shape1 = 3, shape2 = 9, faster = TRUE, family = "beta")
  expect_equal(
    mlEstimationTruncDist(beta_1),
    c("shape1" = 3, "shape2" = 9),
    tolerance = 1e-2
  )
  expect_equal(
    attributes(beta_1),
    list(
      "class" = "trunc_beta",
      "parameters" = list("shape1" = 3, "shape2" = 9),
      "truncation_limits" = list("a" = 0, "b" = 1),
      "continuous" = TRUE
    )
  )

  # Chisq
  chisq_1 <- rtrunc(1e5, df = 30, faster = TRUE, family = "chisq")
  expect_equal(
    attributes(chisq_1),
    list(
      "class" = "trunc_chisq",
      "parameters" = list("df" = 30),
      "truncation_limits" = list("a" = 0, "b" = Inf),
      "continuous" = TRUE
    )
  )
  expect_equal(
    mlEstimationTruncDist(chisq_1),
    c("df" = 30),
    tolerance= 1e-2
  )
})

test_that("Truncation is not a speed limiter", {
  time_limit <- 2
  n <- 1e4L

  # Normal
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, mean = 5, sd = .2, a = 5.5)
    },
    "reached CPU time limit"
  )
  expect_length(rtrunc(n, mean = 5, sd = .2, a = 5.5, faster = TRUE), n)

  # Beta
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "beta", shape1 = 4, shape2 = 5, b = .02)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "beta", shape1 = 4, shape2 = 5, b = .02, faster = TRUE),
    n
  )

  # Chisq
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "chisq", df = 10, b = 1)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "chisq", df = 10, b = 1, faster = TRUE),
    n
  )
})
