# ======================================================== #
# rtrunc methods vs aliases                                #
# ======================================================== #

context("Matching output of rtrunc aliases")

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

# ======================================================== #
# rtrunc functions vs stats functions                      #
# ======================================================== #

context("Matching output of stats::r*")

test_that("Output of rtrunc matches stats::r*", {
	expect_setequal(
		object   = {set.seed(1); rnorm(500, mean=1, sd=3)},
		expected = {set.seed(1); rtrunc(500, mean=1, sd=3)}
	)
	expect_setequal(
		object   = {set.seed(1); rbinom(50, size=10, prob=.3)},
		expected = {set.seed(1); rtrunc(50, size=10, prob=.3, family="binomial")}
	)
	expect_setequal(
		object   = {set.seed(1); rgamma(50, shape=4, rate=5)},
		expected = {set.seed(1); rtrunc(50, shape=4, rate=5, family="gamma")}
	)
	expect_setequal(
		object   = {set.seed(1); rlnorm(50, meanlog=7, sdlog=2)},
		expected = {set.seed(1); rtrunc(50, meanlog=7, sdlog=2, family="lognormal")}
	)
	expect_setequal(
		object   = {set.seed(1); rpois(500, lambda=72)},
		expected = {set.seed(1); rtrunc(500, lambda=72, family="poisson")}
	)
	# TODO: add test for remaining distributions (waiting on #30)
})
