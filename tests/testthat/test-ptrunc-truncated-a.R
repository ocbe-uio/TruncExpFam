context("ptrunc(), lower truncation")

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

test_that("lower-truncation works as expected (binomial)", {
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
