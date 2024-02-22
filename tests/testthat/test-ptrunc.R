context("ptrunc(), untruncated")

test_that("untruncated ptrunc() works as expected (normal)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(10)) {
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
      for (i in seq_len(10)) {
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
      for (i in seq_len(10)) {
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

context("ptrunc(), truncated")

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

test_that("upper-truncation works as expected (normal)", {
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

test_that("upper-truncation works as expected (beta)", {
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

test_that("lower-truncation works as expected (normal)", {
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

test_that("lower-truncation works as expected (beta)", {
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

test_that("Basic errors are caught", {
  for (distro in c("normal", "beta")) { # TODO: eventually use valid_distros
    expect_error(ptrunc(2, distro, 1, 1, a = 3, b = 4), "must be in \\[a, b\\]")
    expect_error(ptrunc(2, distro, 1, 1, a = 0, b = 1), "must be in \\[a, b\\]")
    expect_error(ptrunc(2, distro, 1, 1, a = 3, b = 1), "a must be <= b")
  }
})
