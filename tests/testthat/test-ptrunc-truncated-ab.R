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
