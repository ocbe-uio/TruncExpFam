context("Support validation")

test_that("Impossible truncation limits are rejected", {
  sub <- "must be a subset"
  higher <- "must be higher"
  same <- "Identical truncation limits"
  expect_error(rtrunc(1, family="beta", shape1=8, shape2=2, a=.5, b=.5), same)
  expect_error(rtrunc(1, family="beta", shape1=8, shape2=2, a=1, b=.7), sub)
  expect_error(rtruncbeta(1, shape1=8, shape2=2, a=1, b=.7), sub)
  expect_error(rtruncbeta(1, shape1=8, shape2=2, a=.7, b=.6), higher)
  expect_error(rtruncbeta(1, shape1=8, shape2=2, a=.6, b=.6), same)
  expect_error(rtruncexp(1, rate=8, a=-1, b=0), sub)
  expect_error(rtruncexp(1, rate=8, a=1, b=0.5), higher)
  expect_error(rtruncexp(1, rate=8, a=0.5, b=0.5), same)
})

test_that("Redundant truncation limits are detected", {
  msg <- "are not a subset"
  expect_warning(rtruncbeta(1, shape1=8, shape2=2, a=-0.1), msg)
  expect_warning(rtruncbeta(1, shape1=8, shape2=2, b=1.1), msg)
  expect_warning(rtruncexp(1, rate=8, a=-7), msg)
})
