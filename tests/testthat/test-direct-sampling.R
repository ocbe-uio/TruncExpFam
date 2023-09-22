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

  # Contbern
  contbern_1 <- rtrunc(1e5, lambda = .6, faster = TRUE, family = "contbern")
  expect_equal(
    attributes(contbern_1),
    list(
      "class" = "trunc_contbern",
      "parameters" = list("lambda" = .6),
      "truncation_limits" = list("a" = 0, "b" = 1),
      "continuous" = TRUE
    )
  )
  expect_equal(
    mlEstimationTruncDist(contbern_1),
    c("lambda" = .6),
    tolerance= 1e-1
  )

  # Exp
  exp_1 <- rtrunc(1e5, rate = 64, faster = TRUE, family = "exp")
  expect_equal(
    attributes(exp_1),
    list(
      "class" = "trunc_exp",
      "parameters" = list("rate" = 64),
      "truncation_limits" = list("a" = 0, "b" = Inf),
      "continuous" = TRUE
    )
  )
  expect_equal(
    mlEstimationTruncDist(exp_1),
    c("rate" = 1 / 64),
    tolerance= 1e-2
  )

  # Gamma
  gamma_1 <- rtrunc(1e5, rate = 3, shape = 4, faster = TRUE, family = "gamma")
  expect_equal(
    attributes(gamma_1),
    list(
      "class" = "trunc_gamma",
      "parameters" = list("shape" = 4, "rate" = 3),
      "truncation_limits" = list("a" = 0, "b" = Inf),
      "continuous" = TRUE
    )
  )
  expect_equal(
    mlEstimationTruncDist(gamma_1),
    c("shape" = 4, "rate" = 3),
    tolerance= 1e-1
  )

  gamma_2 <- rtrunc(1e5, scale = 3, shape = 4, faster = TRUE, family = "gamma")
  expect_equal(
    attributes(gamma_2),
    list(
      "class" = "trunc_gamma",
      "parameters" = list("shape" = 4, "rate" = 1 / 3),
      "truncation_limits" = list("a" = 0, "b" = Inf),
      "continuous" = TRUE
    )
  )
  expect_equal(
    mlEstimationTruncDist(gamma_2),
    c("shape" = 4, "rate" = 1 / 3),
    tolerance= 1e-1
  )

  # Inv Gamma
  invgamma_1 <- rtrunc(1e5, rate = 3, shape = 4, faster = TRUE, family = "invgamma")
  expect_equal(
    attributes(invgamma_1),
    list(
      "class" = "trunc_invgamma",
      "parameters" = list("shape" = 4, "rate" = 3),
      "truncation_limits" = list("a" = 0, "b" = Inf),
      "continuous" = TRUE
    )
  )
  expect_equal(
    mlEstimationTruncDist(invgamma_1),
    c("shape" = 4, "rate" = 3),
    tolerance= 1e-1
  )

  invgamma_2 <- rtrunc(1e5, scale = 3, shape = 4, faster = TRUE, family = "invgamma")
  expect_equal(
    attributes(invgamma_2),
    list(
      "class" = "trunc_invgamma",
      "parameters" = list("shape" = 4, "rate" = 1 / 3),
      "truncation_limits" = list("a" = 0, "b" = Inf),
      "continuous" = TRUE
    )
  )
  expect_equal(
    mlEstimationTruncDist(invgamma_2),
    c("shape" = 4, "rate" = 1 / 3),
    tolerance= 1e-1
  )

  # Inv Gauss
  invgauss <- rtrunc(1e5, m = 61, s = 7, faster = TRUE, family = "invgauss")
  expect_equal(
    attributes(invgauss),
    list(
      "class" = "trunc_invgauss",
      "parameters" = list("m" = 61, "s" = 7),
      "truncation_limits" = list("a" = 0, "b" = Inf),
      "continuous" = TRUE
    )
  )
  expect_equal(
    mlEstimationTruncDist(invgauss, delta = .01),
    c("m" = 61, "s" = 7),
    tolerance= 1e-1
  )

  # Log-normal
  invlnorm <- rtrunc(1e5, meanlog = 4, sdlog = 1, faster = TRUE, family = "lognormal")
  expect_equal(
    attributes(invlnorm),
    list(
      "class" = "trunc_lognormal",
      "parameters" = list("meanlog" = 4, "sdlog" = 1),
      "truncation_limits" = list("a" = 0, "b" = Inf),
      "continuous" = TRUE
    )
  )
  expect_equal(
    mlEstimationTruncDist(invlnorm),
    c("meanlog" = 4, "sdlog" = 1),
    tolerance= 1e-1
  )
})

Sys.setenv("LANGUAGE" = "en")

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

  # Contbern
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "contbern", lambda = .8, a = .1, b = .2)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "contbern", lambda = .8, a = .1, b = .2, faster = TRUE),
    n
  )

  # Exp
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, rate = .2, family = "exp", b = .1)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, rate = .2, family = "exp", b = .1, faster = TRUE),
    n
  )

  # Gamma
  expect_error({
      setTimeLimit(time_limit)
      summary(rtrunc(n, family = "gamma", shape = 5, rate = 4, a = 4))
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "gamma", shape = 5, rate = 4, a = 4, faster = TRUE),
    n
  )

  # Inv Gamma
  expect_error({
      setTimeLimit(time_limit)
      summary(rtrunc(n, family = "invgamma", shape = 5, rate = 4, a = 4))
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "invgamma", shape = 5, rate = 4, a = 4, faster = TRUE),
    n
  )

  # Inv Gauss
  expect_error({
      setTimeLimit(time_limit)
      summary(rtrunc(n, family = "invgauss", m = 5, s = 4, a = 4, b = 100))
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "invgauss", m = 5, s = 4, a = 4, b = 100, faster = TRUE),
    n
  )

  # Lognormal
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "lognormal", meanlog = 5, sdlog = 2, a = 150, b = 160)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(
      n, family = "lognormal", meanlog = 5, sdlog = 2, a = 150, b = 160,
      faster = TRUE
    ),
    n
  )
})

setTimeLimit(Inf)
