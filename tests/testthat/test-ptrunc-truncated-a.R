context("ptrunc(), lower truncation")

test_that("lower truncation works as expected (normal)", {
  lt <- TRUE
  lg <- FALSE
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(1)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        qt <- rnorm(1L, mn, sg)
        a <- qt - rchisq(1L, 5L)
        p_trunc <- ptrunc(
          qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg, a = a
        )
        p_norm <- pnorm(qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
          if (lt) {
            expect_lte(p_trunc, p_norm)
          } else {
            expect_gte(p_trunc, p_norm)
          }
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("lower truncation works as expected (beta)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        list2env(
          setNames(as.list(sort(rbeta(2L, shp1, shp2))), c("a", "qt")),
          envir = .GlobalEnv
        )
        p_trunc <- ptrunc(
          qt, "beta", shp1, shp2, a = a, lower.tail = lt, log.p = lg
        )
        p_beta <- pbeta(qt, shp1, shp2, ncp = 0, lt, lg)
        if (!lg) {
          expect_gte(p_trunc, 0)
          expect_lte(p_trunc, 1)
          if (lt) {
            expect_lte(p_trunc, p_beta)
          } else {
            expect_gt(p_trunc, p_beta)
          }
        } else {
          expect_lte(p_trunc, 0)
        }
      }
    }
  }
})

test_that("lower truncation works as expected (binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        size <- sample(10:50, 1L)
        prob <- runif(1)
        a <- sample(1:(size - 4L), 1L)
        qt <- sample(seq(a + 1L, size - 1L), 1L)
        p_trunc <- ptrunc(
          qt, "binomial", size, prob, a = a, lower.tail = lt, log.p = lg
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

test_that("lower truncation works as expected (poisson)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        lambda <- sample(10:50, 1L)
        max_qt <- qpois(p = .99, lambda)
        a <- sample(seq(1L, max_qt - 3L), 1L)
        qt <- sample(seq(a + 1L, max_qt), 1L)
        p_trunc <- ptrunc(
          qt, "poisson", lambda, a = a, lower.tail = lt, log.p = lg
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

test_that("lower truncation works as expected (chisq)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        df <- sample(1:100, 1L)
        a <- min(rchisq(10L, df))
        qt <- max(rchisq(10L, df), a)
        p_trunc <- ptrunc(
          qt, "chisq", df, a = a, lower.tail = lt, log.p = lg
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

test_that("lower truncation works as expected (contbern)", {
  for (i in seq_len(10)) {
    lambda <- runif(1L)
    a <- runif(1L)
    qt <- runif(1L, a, 1L)
    p_trunc <- ptrunc(qt, "contbern", lambda, a = a)
    p_contbern <- pcontbern(qt, lambda)
    expect_lte(p_trunc, p_contbern)
  }
})

test_that("lower truncation works as expected (exp)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        rate <- rchisq(1L, df = 10L)
        a <- rexp(1L, rate)
        qt <- max(rexp(10L, rate), a)
        p_trunc <- ptrunc(
          qt, "exp", rate, a = a, lower.tail = lt, log.p = lg
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

test_that("lower truncation works as expected (gamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        shape <- rchisq(1L, df = 10L)
        rate <- rchisq(1L, df = 10L)
        a <- rgamma(1L, shape, rate)
        qt <- max(rgamma(10L, shape, rate), a)
        p_trunc <- ptrunc(
          qt, "gamma", shape, rate, a = a, lower.tail = lt, log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "gamma", shape, scale = 1 / rate, a = a, lower.tail = lt,
          log.p = lg
        )
        p_gamma <- pgamma(qt, shape, rate, lower.tail = lt, log.p = lg)
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

test_that("lower truncation works as expected (invgamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
        shape <- rchisq(1L, df = 10L)
        rate <- rchisq(1L, df = 10L)
        a <- rinvgamma(1L, shape, rate)
        qt <- max(rinvgamma(10L, shape, rate), a)
        p_trunc <- ptrunc(
          qt, "invgamma", shape, rate, a = a, lower.tail = lt, log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "invgamma", shape, scale = 1 / rate, a = a, lower.tail = lt,
          log.p = lg
        )
        p_invgamma <- pinvgamma(qt, shape, rate, lower.tail = lt, log.p = lg)
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
