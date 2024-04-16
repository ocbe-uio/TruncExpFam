context("ptrunc(), untruncated")

test_that("untruncated ptrunc() works as expected (normal)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(5)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        qt <- rnorm(i, mn, sg)
        p_trunc <- ptrunc(qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg)
        p_norm <- pnorm(qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg)
        for (q in seq_along(qt)) {
          if (!lg) {
            # because I couldn't figure out the relationship between p_trunc
            # and p_norm in the log.p = TRUE case
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_norm[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (beta)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(5)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        qt <- rbeta(i, shp1, shp2)
        p_trunc <- ptrunc(qt, "beta", shp1, shp2, lower.tail = lt, log.p = lg)
        p_beta <- pbeta(qt, shp1, shp2, ncp = 0, lt, lg)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_beta[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(5)) {
        size <- sample(1:10, 1L)
        prob <- runif(1)
        qt <- rbinom(i, size, prob)
        p_trunc <- ptrunc(qt, "binomial", size, prob, lower.tail = lt, log.p = lg)
        p_binom <- pbinom(qt, size, prob, lower.tail = lt, log.p = lg)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_binom[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (poisson)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(5)) {
        lambda <- sample(1:50, 1L)
        qt <- rpois(i, lambda)
        p_trunc <- ptrunc(qt, "poisson", lambda, lower.tail = lt, log.p = lg)
        p_pois <- ppois(qt, lambda, lower.tail = lt, log.p = lg)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_pois[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (chisq)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(5)) {
        df <- sample(1:10, 1L)
        qt <- rchisq(i, df)
        p_trunc <- ptrunc(qt, "chisq", df, lower.tail = lt, log.p = lg)
        p_chisq <- pchisq(qt, df, lower.tail = lt, log.p = lg)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_chisq[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (contbern)", {
  for (i in seq_len(5)) {
    lambda <- runif(1)
    qt <- rcontbern(i, lambda)
    p_trunc <- ptrunc(qt, "contbern", lambda)
    p_contbern <- pcontbern(qt, lambda)
    for (q in seq_along(qt)) {
      expect_equal(p_trunc[q], p_contbern[q])
    }
  }
})

test_that("untruncated ptrunc() works as expected (exp)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(5)) {
        rate <- runif(1)
        qt <- rexp(i, rate)
        p_trunc <- ptrunc(qt, "exp", rate, lower.tail = lt, log.p = lg)
        p_exp <- pexp(qt, rate, lower.tail = lt, log.p = lg)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_exp[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (gamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(5)) {
        shp <- rchisq(1L, df = 10L)
        rate <- rchisq(1L, df = 10L)
        qt <- rgamma(i, shp, rate)
        p_trunc <- ptrunc(qt, "gamma", shp, rate, lower.tail = lt, log.p = lg)
        p_trunc_2 <- ptrunc(
          qt, "gamma", shp, scale = 1 / rate, lower.tail = lt, log.p = lg
        )
        p_gamma <- pgamma(qt, shp, rate, lower.tail = lt, log.p = lg)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            expect_gte(p_trunc_2[q], 0)
            expect_lte(p_trunc_2[q], 1)
          }
          expect_equal(p_trunc[q], p_gamma[q])
          expect_equal(p_trunc_2[q], p_gamma[q])
        }
        expect_equal(p_trunc, p_trunc_2)
      }
    }
  }
})

test_that("Basic errors are caught", {
  for (distro in c("normal", "beta", "binomial", "poisson", "chisq", "contbern", "exp", "gamma")) { # TODO: eventually use valid_distros
    expect_error(ptrunc(2, distro, 1, 1, a = 3, b = 4), "must be in \\[a, b\\]")
    expect_error(ptrunc(2, distro, 1, 1, a = 0, b = 1), "must be in \\[a, b\\]")
    expect_error(ptrunc(2, distro, 1, 1, a = 3, b = 1), "a must be <= b")
  }
})
