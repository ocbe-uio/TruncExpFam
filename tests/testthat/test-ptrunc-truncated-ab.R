context("ptrunc(), doubly truncated")

test_that("doubly-truncated ptrunc works as expected (normal)", {
  lt <- TRUE
  lg <- FALSE
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        qt <- rnorm(1L, mn, sg)
        a <- qt - rchisq(1L, 5L)
        b <- qt + rchisq(1L, 5L)
        p_trunc <- ptrunc(
          qt, "gaussian", mn, sg, a, b, lower.tail = lt, log.p = lg
        )
        p_norm <- pnorm(qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("doubly-truncated ptrunc() works as expected (beta)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        b <- runif(1)
        a <- b * runif(1)
        qt <- runif(1L, a, b)
        p_trunc <- ptrunc(
          qt, "beta", shp1, shp2, a, b, lower.tail = lt, log.p = lg
        )
        p_beta <- pbeta(qt, shp1, shp2, ncp = 0, lt, lg)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("doubly-truncated ptrunc() works as expected (binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        size <- sample(10:50, 1L)
        prob <- runif(1)
        a <- sample(1:(size - 4L), 1L)
        b <- sample((a + 3L):size, 1L)
        qt <- sample(seq(a + 1L, b - 1L), 1L)
        p_trunc <- ptrunc(
          qt, "binomial", size, prob, a, b, lower.tail = lt, log.p = lg
        )
        p_binom <- pbinom(qt, size, prob, lower.tail = lt, log.p = lg)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("doubly-truncated ptrunc() works as expected (poisson)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        lambda <- sample(1:50, 1L)
        a <- sample(1:(lambda - 4L), 1L)
        b <- sample((a + 3L):lambda, 1L)
        qt <- sample(seq(a + 1L, b - 1L), 1L)
        p_trunc <- ptrunc(
          qt, "poisson", lambda, a, b, lower.tail = lt, log.p = lg
        )
        p_pois <- ppois(qt, lambda, lower.tail = lt, log.p = lg)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("doubly-truncated ptrunc() works as expected (chisq)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        df <- sample(1:100, 1L)
        a <- min(rchisq(10L, df))
        b <- max(rchisq(10L, df))
        qt <- runif(1L, a, b)
        p_trunc <- ptrunc(
          qt, "chisq", df, a, b, lower.tail = lt, log.p = lg
        )
        p_chisq <- pchisq(qt, df, ncp = 0, lower.tail = lt, log.p = lg)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("doubly-truncated ptrunc() works as expected (contbern)", {
  for (i in seq_len(10)) {
    lambda <- runif(1L)
    a <- runif(1L)
    b <- runif(1L, a, 1L)
    qt <- runif(1L, a, b)
    p_trunc <- ptrunc(qt, "contbern", lambda, b = b)
    p_contbern <- pcontbern(qt, lambda)
    expect_gte(p_trunc, p_contbern)
  }
})

test_that("doubly-truncated ptrunc() works as expected (exp)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        rate <- rchisq(1L, df = 10L)
        a <- rexp(1L, rate)
        b <- max(rexp(10L, rate), a)
        qt <- runif(1L, a, b)
        p_trunc <- ptrunc(
          qt, "exp", rate, a, b, lower.tail = lt, log.p = lg
        )
        p_exp <- pexp(qt, rate, lower.tail = lt, log.p = lg)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("doubly-truncated ptrunc() works as expected (gamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        shp <- rchisq(1L, df = 10L)
        rte <- rchisq(1L, df = 10L)
        ab <- rgamma(2L, shp, rte)
        a <- min(ab)
        b <- max(ab)
        qt <- runif(1L, a, b)
        p_trunc <- ptrunc(
          qt, "gamma", shape = shp, rate = rte, a = a, b = b, lower.tail = lt, log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "gamma", shape = shp, scale = 1 / rte, a = a, b = b,
          lower.tail = lt, log.p = lg
        )
        p_gamma <- pgamma(qt, shape = shp, rate = rte, lower.tail = lt, log.p = lg)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
          expect_gte(p_trunc_2, 0)
          expect_lte(p_trunc_2, 1)
        } else {
          expect_lte(p_trunc, 0)
          expect_lte(p_trunc_2, 0)
        }
        expect_equal(p_trunc, p_trunc_2)
      }
    }
  }
})
