context("qtrunc, lower truncation")

test_that("qtrunc() works as expected (beta)", {
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        a <- qtrunc(min(pt) / 2, "beta", shp1, shp2, lower.tail = lt, log.p = lg)
        q_trunc <- qtrunc(
          pt, "beta", shp1, shp2, a = a, lower.tail = lt, log.p = lg
        )
        q_stats <- qbeta(pt, shp1, shp2, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_gt(q_trunc[ii], q_stats[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], "beta", shp1, shp2, lower.tail = lt, log.p = lg, a = a
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
        if (lg) pt <- log(pt)
        a <- qtrunc(min(pt) / 2, fam, sz, pb, lower.tail = lt, log.p = lg)
        q_trunc <- qtrunc(pt, fam, sz, pb, a = a, lower.tail = lt, log.p = lg)
        q_stats <- qbinom(pt, sz, pb, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_gte(q_trunc[ii], q_stats[ii])
          # Working back to p from q
          q_lo <- max(q_trunc[ii] - 1L, 0L, a)
          q_hi <- min(q_trunc[ii] + 1L, sz)
          ptr_1 <- ptrunc(q_lo, fam, sz, pb, a = a, lower.tail = lt, log.p = lg)
          ptr_2 <- ptrunc(q_hi, fam, sz, pb, a = a, lower.tail = lt, log.p = lg)
          # because pt will have been rounded
          if (q_trunc[ii] > 0L && q_lo > a) {
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

test_that("qtrunc() works as expected (chisq)", {
  fam <- "chisq"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        df <- sample(1:10, 1L)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        a <- min(qtrunc(pt, fam, df, lower.tail = lt, log.p = lg) / 2)
        q_trunc <- qtrunc(pt, fam, df, lower.tail = lt, log.p = lg, a = a)
        q_stats <- qchisq(pt, df, lower.tail = lt, log.p = lg)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_gte(q_trunc[ii], q_stats[ii])
          # Working back to p from q
          ptr <- ptrunc(q_trunc[ii], fam, df, lower.tail = lt, log.p = lg, a = a)
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (contbern)", {
  fam <- "contbern"
  for (i in seq_len(3L)) {
    lb <- runif(1L)
    pt <- runif(i)
    a <- runif(1L)
    q_trunc <- qtrunc(pt, fam, lb, a = a)
    q_stats <- qcontbern(pt, lb)
    expect_length(q_trunc, i)
    for (ii in seq_along(pt)) {
      expect_gt(q_trunc[ii], q_stats[ii])
      # Working back to p from q
      ptr <- ptrunc(q_trunc[ii], fam, lb, a = a)
      expect_equal(pt[ii], ptr)
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
        if (lg) pt <- log(pt)
        a <- qtrunc(
          min(pt) / 2, "normal", mean = mn, sd = sg, lower.tail = lt, log.p = lg
        )
        q_trunc <- qtrunc(
          pt, "normal", mean = mn, sd = sg, a = a, lower.tail = lt, log.p = lg
        )
        q_norm <- qnorm(pt, mean = mn, sd = sg, lower.tail = lt, log.p = lg)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_gt(q_trunc[ii], q_norm[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], "normal", mean = mn, sd = sg, a = a,
            lower.tail = lt, log.p = lg
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})
