context("Extracting parameters")

set.seed(1418193)
x <- rtrunc(n = 100, lambda = 2, family = "Poisson")
attributes(x) <- NULL

test_that("Parameters are properly named", {
  expect_named(extractParameters(x, family = "beta"), c("shape1", "shape2"))
  expect_error(extractParameters(x, family = "binomial"), "nsize")
  expect_named(extractParameters(x, family = "binomial", nsize = 3), "prob")
  expect_named(extractParameters(x, family = "poisson"), "lambda")
})

test_that("Natural parameters are properly named", {
  expect_named(extractParameters(x, family = "beta", TRUE), c("eta1", "eta2"))
  expect_error(extractParameters(x, family = "binomial"), "nsize")
  expect_named(extractParameters(x, family = "binomial", nsize = 3, TRUE), "eta")
  expect_named(extractParameters(x, family = "poisson", TRUE), "eta")
})
