context("rtrunc aliases")

test_that("rtrunc works the same from generic and alias", {
	expect_identical(
		object = {set.seed(8); rtrunc(100, 4, .2, family="binomial")},
		expected = {set.seed(8); rtruncbinom(100, 4, .2)},
	)
	expect_identical(
		object = {set.seed(8); rtrunc(100, 45, family="chisq")},
		expected = {set.seed(8); rtruncchisq(100, 45)},
	)
	expect_identical(
		object = {set.seed(8); rtrunc(100, .2, family="contbernoulli")},
		expected = {set.seed(8); rtrunccontbernoulli(100, .2)},
	)
	expect_identical(
		object = {set.seed(8); rtrunc(100, 45, 12, family="gamma")},
		expected = {set.seed(8); rtruncgamma(100, 45, 12)},
	)
	expect_identical(
		object = {set.seed(8); rtrunc(100, -32, 2, family="lognormal")},
		expected = {set.seed(8); rtrunclnorm(100, -32, 2)},
	)
	expect_identical(
		object = {set.seed(8); rtrunc(100, 100, 200, family="normal")},
		expected = {set.seed(8); rtruncnorm(100, 100, 200)},
	)
	expect_identical(
		object = {set.seed(8); rtrunc(100, 7521, family="poisson")},
		expected = {set.seed(8); rtruncpois(100, 7521)},
	)
})