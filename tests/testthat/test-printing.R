library(TruncExpFam) #TEMP
library(testthat) #TEMP

context("Printing")

set.seed(4354)
x_beta <- rtrunc(10, family="beta", shape1=3, shape2=2)
x_bino <- rtrunc(10, family="binomial", size=3, prob=.2)
x_chis <- rtrunc(10, family="chisq", df=12)
x_cber <- rtrunc(10, family="contbern", lambda=.8)
x_expo <- rtrunc(10, family="exp", rate=12)
x_gam1 <- rtrunc(10, family="gamma", shape=8, rate=5)
x_gam2 <- rtrunc(10, family="gamma", shape=8, scale=5)
x_igm1 <- rtrunc(10, family="invgamma", shape=8, rate=5)
x_igm2 <- rtrunc(10, family="invgamma", shape=8, scale=5)
x_inor <- rtrunc(10, family="invgauss", m=5, s=4)
x_norm <- rtrunc(10, family="normal", mean=23, sd=5)
x_pois <- rtrunc(10, family="poisson", lambda=85)

test_that("Printing options are respected", {

  # Check if default printing hides attributes ------------- #

  expect_equal(capture.output(attributes(print(x_beta)))[2], "NULL")
  expect_equal(capture.output(attributes(print(x_bino)))[2], "NULL")
  expect_equal(capture.output(attributes(print(x_chis)))[2], "NULL")
  expect_equal(capture.output(attributes(print(x_cber)))[2], "NULL")
  expect_equal(capture.output(attributes(print(x_expo)))[2], "NULL")
  expect_equal(capture.output(attributes(print(x_gam1)))[2], "NULL")
  expect_equal(capture.output(attributes(print(x_gam2)))[2], "NULL")
  expect_equal(capture.output(attributes(print(x_igm1)))[2], "NULL")
  expect_equal(capture.output(attributes(print(x_igm2)))[2], "NULL")
  expect_equal(capture.output(attributes(print(x_inor)))[2], "NULL")
  expect_equal(capture.output(attributes(print(x_norm)))[2], "NULL")
  expect_equal(capture.output(attributes(print(x_pois)))[2], "NULL")

  # Check if details=TRUE displays the attributes ---------- #

  # Each additional distribution parameter adds 3 objects to the printout
  # (arg name + value + space). That's why distribution with two parameters
  # have 12 objects and one-parameter ones have 9.

  expect_length(capture.output(print(x_beta, details=TRUE)), 12)
  expect_length(capture.output(print(x_bino, details=TRUE)), 12)
  expect_length(capture.output(print(x_chis, details=TRUE)), 9)
  expect_length(capture.output(print(x_cber, details=TRUE)), 9)
  expect_length(capture.output(print(x_expo, details=TRUE)), 9)
  expect_length(capture.output(print(x_gam1, details=TRUE)), 12)
  expect_length(capture.output(print(x_gam2, details=TRUE)), 12)
  expect_length(capture.output(print(x_igm1, details=TRUE)), 12)
  expect_length(capture.output(print(x_igm2, details=TRUE)), 12)
  expect_length(capture.output(print(x_inor, details=TRUE)), 12)
  expect_length(capture.output(print(x_norm, details=TRUE)), 12)
  expect_length(capture.output(print(x_pois, details=TRUE)), 9)
})
