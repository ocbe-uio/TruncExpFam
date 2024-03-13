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
