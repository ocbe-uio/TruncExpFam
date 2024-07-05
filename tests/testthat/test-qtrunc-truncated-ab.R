context("qtrunc, upper truncation")

test_that("qtrunc() works as expected (beta)", {
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        pt <- runif(i)
        qt <- c(runif(100L), pt)
        a <- min(qt) - rchisq(1L, 5L)
        b <- max(qt) + rchisq(1L, 5L)
        if (lg) pt <- log(pt)
        q_trunc <- qtrunc(
          pt, "beta", shp1, shp2, a = a, b = b, lower.tail = lt, log.p = lg
        )
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], "beta", shp1, shp2, lower.tail = lt, log.p = lg,
            a = a, b = b
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (binomial)", {
  fam <- "binomial"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        sz <- sample(1:10, 1L)
        pb <- runif(1)
        pt <- runif(i)
        ab_probs <- c(runif(100L), pt)
        b <- qtrunc(max(ab_probs), fam, sz, pb, lower.tail = TRUE, log.p = FALSE)
        a <- qtrunc(min(ab_probs), fam, sz, pb, lower.tail = TRUE, log.p = FALSE)
        if (lg) pt <- log(pt)
        q_trunc <- qtrunc(pt, fam, sz, pb, a = a, b = b, lower.tail = lt, log.p = lg)
        q_stats <- qbinom(pt, sz, pb, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          # Working back to p from q
          q_lo <- max(q_trunc[ii] - 1L, 0L, a)
          q_hi <- min(q_trunc[ii] + 1L, sz, b)
          ptr_1 <- ptrunc(q_lo, fam, sz, pb, a = a, b = b, lower.tail = lt, log.p = lg)
          ptr_2 <- ptrunc(q_hi, fam, sz, pb, a = a, b = b, lower.tail = lt, log.p = lg)
          # because pt will have been rounded
          if (q_trunc[ii] > 0L && q_hi < b && q_lo > a) {
            if (lt) {
              expect_gte(pt[ii], ptr_1)
              expect_lte(pt[ii], ptr_2)
            } else {
              expect_lte(pt[ii], ptr_1)
              expect_gte(pt[ii], ptr_2)
            }
          }
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (normal)", {
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        pt <- runif(i)
        ab <- c(runif(100L), pt)
        b <- qtrunc(max(ab), mean = mn, sd = sg, lower.tail = TRUE, log.p = FALSE)
        a <- qtrunc(min(ab), mean = mn, sd = sg, lower.tail = TRUE, log.p = FALSE)
        if (lg) pt <- log(pt)
        q_trunc <- qtrunc(
          pt, "normal", mean = mn, sd = sg, a = a, b = b,
          lower.tail = lt, log.p = lg
        )
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], "normal", mean = mn, sd = sg, a = a, b = b,
            lower.tail = lt, log.p = lg
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})
