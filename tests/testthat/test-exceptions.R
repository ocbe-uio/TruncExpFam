context("Handling exceptions")

test_that("Wrong distro and parameters are handled correctly", {
	expect_error(rtrunc(10, family='Poison'))
	expect_error(rtrunc(10, family='Poisson', mean=10))
})

test_that("It's OK to miss truncation limits", {
	set.seed(90)
	samp <- list(
		"norm" = rtrunc(3, family = "normal", mean = 0, sd = 1),
		"gamm" = rtrunc(3, family = "gamma", shape = 5, rate = 1),
		"logn" = rtrunc(3, family = "lognormal", meanlog = 0, sdlog = 1),
		"pois" = rtrunc(3, family = "poisson", lambda = 1)
	)
	expect_equal(
		object   = dtrunc(samp$norm, eta = c(10, -2)),
		expected = c(6.357392e-06, 6.271760e-07, 9.013280e-11)
	)
	expect_equal(
		object   = dtrunc(samp$gamm, eta = c(10, -2)),
		expected = c(0.09401543, 0.23025945, 0.14131485)
	)
	expect_equal(
		object   = dtrunc(samp$logn, eta = c(10, -2)),
		expected = c(2.094700e-03, 9.567618e-09, 7.123690e-02)
	)

	expect_equal(
		object   =  dtrunc(samp$pois, eta = -10),
		expected = c(9.999546e-01, 1.030530e-09, 4.539787e-05)
	)
})
