context("Handling exceptions")

test_that("Wrong distro and parameters are handled correctly", {
	expect_error(rtrunc(10, family='Poison'))
	expect_error(rtrunc(10, family='Poisson', mean=10))
})