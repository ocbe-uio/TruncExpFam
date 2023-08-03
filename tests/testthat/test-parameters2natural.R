context("Converting numeric vectors to natural parameters and back")

test_that("Wrong combinations trigger errors", {
  expect_error(parameters2natural(c(1, 3)), "provide a named parameter vector")
  expect_error(parameters2natural(c(mean = 1, var = 3)), "set does not match")
  expect_error(parameters2natural(c(mean = 1, sd = 3), "beta"), "parameter set")
  expect_error(natural2parameters(c(mean = 1, eta = 3)), "start with \"eta\"")
  expect_error(natural2parameters(c(1)), "must be a vector of two elements")
})

test_that("Converting to natural", {
  expect_equivalent(
    unclass(parameters2natural(c(shape1 = 7, shape2 = 3), "beta")),
    c(eta1 = 7, eta2 = 3)
  )
  expect_equivalent(
    unclass(parameters2natural(c(size = 7, prob = .3), "binomial")),
    c(eta = log(.3 / .7))
  )
  expect_equivalent(
    unclass(parameters2natural(c(df = 7), "chisq")),
    c(eta = 7 / 2 - 1)
  )
  expect_equivalent(
    unclass(parameters2natural(c(lambda = .7), "contbern")),
    c(eta = log(.7 / .3))
  )
  expect_equivalent(
    unclass(parameters2natural(c(rate = 7), "exp")),
    c(eta = -7)
  )
  expect_equivalent(
    unclass(parameters2natural(c(shape = 7, rate = 3), "gamma")),
    c(eta1 = 7 - 1, eta2 = -3)
  )
  expect_equivalent(
    unclass(parameters2natural(c(shape = 7, scale = 3), "gamma")),
    c(eta1 = 7 - 1, eta2 = - 1 / 3)
  )
  expect_equivalent(
    unclass(parameters2natural(c(shape = 7, rate = 3), "invgamma")),
    c(eta1 = - 7 - 1, eta2 = -3)
  )
  expect_equivalent(
    unclass(parameters2natural(c(m = 7, s = 3), "invgauss")),
    c(eta1 = - (1 / 3) / 2 / 7 ^ 2, eta2 = - (1 / 3) / 2)
  )
  expect_equivalent(
    unclass(parameters2natural(c(meanlog = 7, sdlog = 3), "lognormal")),
    c(eta1 = 7 / 3 ^ 2, eta2 = - 1 / 2 / 3 ^ 2)
  )
  expect_equivalent(
    unclass(parameters2natural(c(mean = 7, sd = 3), "normal")),
    c(eta1 = 7 / 3 ^ 2, eta2 = - 1 / 2 / 3 ^ 2)
  )
  expect_equivalent(
    unclass(parameters2natural(c(lambda = 7), "poisson")),
    c(eta = log(7))
  )
})

test_that("Converting from natural", {
  expect_equivalent(
    unclass(natural2parameters(c(eta1 = 7, eta2 = 3), "beta")),
    c(shape1 = 7, shape2 = 3)
  )
  expect_equivalent(
    unclass(natural2parameters(c(eta = log(.3 / .7)), "binomial")),
    c(prob = .3)
  )
  expect_equivalent(
    unclass(natural2parameters(c(eta = 7 / 2 - 1), "chisq")),
    c(df = 7)
  )
  expect_equivalent(
    unclass(natural2parameters(c(eta = log(.7 / .3)), "contbern")),
    c(lambda = .7)
  )
  expect_equivalent(
    unclass(natural2parameters(c(eta = -7), "exp")),
    c(rate = 7)
  )
  expect_equivalent(
    unclass(natural2parameters(c(eta1 = 7 - 1, eta2 = -3), "gamma")),
    c(shape = 7, rate = 3)
  )
  expect_equivalent(
    unclass(natural2parameters(c(eta1 = - 7 - 1, eta2 = -3), "invgamma")),
    c(shape = 7, rate = 3)
  )
  expect_equivalent(
    unclass(natural2parameters(c(eta1 = - (1 / 3) / 2 / 7 ^ 2, eta2 = - (1 / 3) / 2), "invgauss")),
    c(m = 7, s = 3)
  )
  expect_equivalent(
    unclass(natural2parameters(c(eta1 = 7 / 3 ^ 2, eta2 = - 1 / 2 / 3 ^ 2), "lognormal")),
    c(meanlog = 7, sdlog = 3)
  )
  expect_equivalent(
    unclass(natural2parameters(c(eta1 = 7 / 3 ^ 2, eta2 = - 1 / 2 / 3 ^ 2), "normal")),
    c(mean = 7, sd = 3)
  )
  expect_equivalent(
    unclass(natural2parameters(c(eta = log(7)), "poisson")),
    c(lambda = 7)
  )
})
