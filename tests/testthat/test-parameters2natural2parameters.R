context("Extracting parameters")

set.seed(1418193)
x <- rtrunc(n = 100, lambda = 2, family = "Poisson")
attributes(x) <- NULL
y <- x / 10

test_that("Parameters are properly named", {
  expect_error(extractParameters(x, family = "beta"), "outside of support")
  expect_named(extractParameters(y, family = "beta"), c("shape1", "shape2"))
  expect_error(extractParameters(x, family = "binomial", nsize = 3), "outside of support")
  expect_named(extractParameters(x, family = "binomial", nsize = max(x)), c("size", "prob"))
  expect_named(extractParameters(x, family = "chisq"), "df")
  expect_error(extractParameters(x, family = "contbern"), "outside of support")
  expect_named(extractParameters(x, family = "poisson"), "lambda")
})

test_that("Natural parameters are properly named", {
  expect_named(extractParameters(y, family = "beta", TRUE), c("eta1", "eta2"))
  expect_named(extractParameters(x, family = "binomial", TRUE), "eta")
  expect_named(extractParameters(x, family = "chisq", TRUE), "eta")
  expect_named(extractParameters(x, family = "poisson", TRUE), "eta")
})
