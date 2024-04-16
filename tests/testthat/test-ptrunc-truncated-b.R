context("ptrunc(), upper truncation")

test_that("upper truncation works as expected (normal)", {
  lt <- TRUE
  lg <- FALSE
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(1)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        qt <- rnorm(1L, mn, sg)
        b <- qt + rchisq(1L, 5L)
        p_trunc <- ptrunc(
          qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg, b = b
        )
        p_norm <- pnorm(qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg)
        expect_length(qt, i)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
          if (lt) {
            expect_gte(p_trunc, p_norm)
          } else {
            expect_lte(p_trunc, p_norm)
          }
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("upper truncation works as expected (beta)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        b <- runif(1)
        qt <- runif(1L, 0, b)
        p_trunc <- ptrunc(
          qt, "beta", shp1, shp2, b = b, lower.tail = lt, log.p = lg
        )
        p_beta <- pbeta(qt, shp1, shp2, ncp = 0, lt, lg)
        expect_length(qt, i)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
          if (lt) {
            expect_gte(p_trunc, p_beta)
          } else {
            expect_lte(p_trunc, p_beta)
          }
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("upper truncation works as expected (binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        size <- sample(10:50, 1L)
        prob <- runif(1)
        b <- sample(2:(size - 1L), 1L)
        qt <- sample(0:(b - 1L), 1L)
        p_trunc <- ptrunc(
          qt, "binomial", size, prob, b = b, lower.tail = lt, log.p = lg
        )
        p_binom <- pbinom(qt, size, prob, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
          if (abs(p_trunc - p_binom) > 1e-10) {  # adding tolerance
            if (lt) {
              expect_gte(p_trunc, p_binom)
            } else {
              expect_lte(p_trunc, p_binom)
            }
          }
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("upper truncation works as expected (poisson)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        lambda <- sample(10:50, 1L)
        max_qt <- qpois(p = .99, lambda)
        b <- sample(seq(lambda, max_qt), 1L)
        qt <- sample(seq(1L, b - 1L), 1L)
        p_trunc <- ptrunc(
          qt, "poisson", lambda, b = b, lower.tail = lt, log.p = lg
        )
        p_pois <- ppois(qt, lambda, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
          if (abs(p_trunc - p_pois) > 1e-10) {  # adding tolerance
            if (lt) {
              expect_gte(p_trunc, p_pois)
            } else {
              expect_lte(p_trunc, p_pois)
            }
          }
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("upper truncation works as expected (chisq)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        df <- sample(1:100, 1L)
        b <- max(rchisq(10L, df))
        qt <- runif(1L, 0, b)
        p_trunc <- ptrunc(
          qt, "chisq", df, b = b, lower.tail = lt, log.p = lg
        )
        p_chisq <- pchisq(qt, df, ncp = 0, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
          if (abs(p_trunc - p_chisq) > 1e-10) {  # adding tolerance
            if (lt) {
              expect_gte(p_trunc, p_chisq)
            } else {
              expect_lte(p_trunc, p_chisq)
            }
          }
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("upper truncation works as expected (contbern)", {
  for (i in seq_len(10)) {
    lambda <- runif(1L)
    b <- runif(1L)
    qt <- runif(1L, 0L, b)
    p_trunc <- ptrunc(qt, "contbern", lambda, b = b)
    p_contbern <- pcontbern(qt, lambda)
    expect_length(qt, i)
    expect_gte(p_trunc, p_contbern)
  }
})

test_that("upper truncation works as expected (exp)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        rate <- rchisq(1L, df = 10L)
        b <- rexp(1L, rate)
        qt <- min(rexp(10L, rate), b)
        p_trunc <- ptrunc(
          qt, "exp", rate, b = b, lower.tail = lt, log.p = lg
        )
        p_exp <- pexp(qt, rate, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
          if (abs(p_trunc - p_exp) > 1e-10) {  # adding tolerance
            if (lt) {
              expect_gte(p_trunc, p_exp)
            } else {
              expect_lte(p_trunc, p_exp)
            }
          }
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("upper truncation works as expected (gamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        shp <- rchisq(1L, df = 10L)
        rte <- rchisq(1L, df = 10L)
        b <- rgamma(1L, shp, rte)
        qt <- runif(1L, 0, b)
        p_trunc <- ptrunc(
          qt, "gamma", shp, rate = rte, b = b, lower.tail = lt, log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "gamma", shp, scale = 1 / rte, b = b, lower.tail = lt, log.p = lg
        )
        p_gamma <- pgamma(qt, shp, rate = rte, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
          expect_gte(p_trunc_2, 0)
          expect_lte(p_trunc_2, 1)
          if (abs(p_trunc - p_gamma) > 1e-10) {  # adding tolerance
            if (lt) {
              expect_gte(p_trunc, p_gamma)
              expect_gte(p_trunc_2, p_gamma)
            } else {
              expect_lte(p_trunc, p_gamma)
              expect_lte(p_trunc_2, p_gamma)
            }
          }
        } else {
          expect_lte(p_trunc, 0)
          expect_lte(p_trunc_2, 0)
        }
        expect_equal(p_trunc, p_trunc_2)
      }
    }
  }
})

test_that("upper truncation works as expected (invgamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        shp <- rchisq(1L, df = 10L)
        rte <- rchisq(1L, df = 10L)
        b <- rinvgamma(1L, shp, rte)
        qt <- runif(1L, 0, b)
        p_trunc <- ptrunc(
          qt, "invgamma", shp, rate = rte, b = b, lower.tail = lt, log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "invgamma", shp, scale = 1 / rte, b = b, lower.tail = lt, log.p = lg
        )
        p_invgamma <- pinvgamma(qt, shp, rate = rte, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
          expect_gte(p_trunc_2, 0)
          expect_lte(p_trunc_2, 1)
          if (abs(p_trunc - p_invgamma) > 1e-10) {  # adding tolerance
            if (lt) {
              expect_gte(p_trunc, p_invgamma)
              expect_gte(p_trunc_2, p_invgamma)
            } else {
              expect_lte(p_trunc, p_invgamma)
              expect_lte(p_trunc_2, p_invgamma)
            }
          }
        } else {
          expect_lte(p_trunc, 0)
          expect_lte(p_trunc_2, 0)
        }
        expect_equal(p_trunc, p_trunc_2)
      }
    }
  }
})

test_that("upper truncation works as expected (invgauss)", {
  for (i in seq_len(5)) {
    m <- rchisq(1L, df = 10L)
    s <- rchisq(1L, df = 10L)
    b <- rinvgauss(1L, m, s)
    qt <- min(rinvgauss(10L, m, s), a)
    p_trunc <- ptrunc(qt, "invgauss", m, s, b = b)
    p_invgauss <- pinvgauss(qt, m, s)
    expect_length(qt, i)
    for (q in seq_along(qt)) {
      expect_gte(p_trunc[q], 0)
      expect_lte(p_trunc[q], 1)
      expect_gte(p_trunc[q], p_invgauss[q])
    }
  }
})
