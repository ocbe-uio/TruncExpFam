context("Checking output of empiricalParameters")

sample_size <- 10
sample <- list(
  "beta"      = rtruncbeta(sample_size, shape1 = 15, shape2 = 4),
  "binomial"  = rtruncbinom(sample_size, prob = 0.6, size = 20),
  "chisq"     = rtruncchisq(sample_size, df = 50),
  "contbern"  = rtrunccontbern(sample_size, lambda = .4),
  "exp"       = rtruncexp(sample_size, rate = 6),
  "gamma"     = rtruncgamma(sample_size, shape = 6, rate = 2, a = 2),
  "invgamma"  = rtruncinvgamma(sample_size, shape = 23, rate = 24),
  "invgauss"  = rtruncinvgauss(sample_size, m = 3, s = 1),
  "lognormal" = rtrunclnorm(sample_size, meanlog = 2.5, sdlog = 0.5),
  "nbinom"    = rtruncnbinom(sample_size, size = 50, prob = .3),
  "normal"    = rtruncnorm(sample_size, mean = 2, sd = 1.5),
  "poisson"   = rtruncpois(sample_size, lambda = 10)
)

test_that("empiricalParameters are properly named", {
  for (distro in names(sample)) {
    expected_parms <- valid_fam_parm[[distro]][["parms"]]
    empirical_parameters <- switch(distro,
      "nbinom" = empiricalParameters(sample[[distro]], r = 50, k = 50),
      empiricalParameters(sample[[distro]])
    )
    expect_named(empirical_parameters, expected_parms)
  }
})

test_that("extractParameters() can replace empiricalParameters()", {
  for (distro in names(sample)) {
    expected_parms <- valid_fam_parm[[distro]][["parms"]]
    samp <- sample[[distro]]
    empirical_parameters <- switch(distro,
      "nbinom" = suppressMessages(extractParameters(samp, r = 50, k = 50)),
      suppressMessages(extractParameters(samp))
    )
    expect_named(empirical_parameters, expected_parms)
  }
})
