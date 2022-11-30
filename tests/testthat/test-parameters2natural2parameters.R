context("Extracting empirical parameters")

set.seed(1418193)
x <- rtrunc(n = 100, lambda = 2, family = "Poisson")
attributes(x) <- NULL
y <- x / 10

test_that("Parameters are properly named", {
  expect_error(empiricalParameters(x, family = "beta"), "outside of support")
  expect_named(empiricalParameters(y, family = "beta"), c("shape1", "shape2"))
  expect_error(empiricalParameters(x, family = "binomial", nsize = 3), "outside of support")
  expect_named(empiricalParameters(x, family = "binomial", nsize = max(x)), c("size", "prob"))
  expect_named(empiricalParameters(x, family = "chisq"), "df")
  expect_error(empiricalParameters(x, family = "contbern"), "outside of support")
  expect_named(empiricalParameters(x, family = "poisson"), "lambda")
})

test_that("Natural parameters are properly named", {
  expect_named(empiricalParameters(y, family = "beta", TRUE), c("eta1", "eta2"))
  expect_named(empiricalParameters(x, family = "binomial", TRUE), "eta")
  expect_named(empiricalParameters(x, family = "chisq", TRUE), "eta")
  expect_named(empiricalParameters(x, family = "poisson", TRUE), "eta")
})
