context("qtrunc, untruncated")

test_that("qtrunc() works as expected (beta)", {
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        q_trunc <- qtrunc(pt, "beta", shp1, shp2, lower.tail = lt, log.p = lg)
        q_stats <- qbeta(pt, shp1, shp2, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_equal(q_trunc[ii], q_stats[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], "beta", shp1, shp2, lower.tail = lt, log.p = lg
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (binomial)", {
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        sz <- sample(1:10, 1L)
        pb <- runif(1)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        q_trunc <- qtrunc(pt, "binomial", sz, pb, lower.tail = lt, log.p = lg)
        q_stats <- qbinom(pt, sz, pb, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_equal(q_trunc[ii], q_stats[ii])
          # Working back to p from q
          q_lo <- max(q_trunc[ii] - 1L, 0L)
          q_hi <- min(q_trunc[ii] + 1L, sz)
          ptr_1 <- ptrunc(q_lo, "binomial", sz, pb, lower.tail = lt, log.p = lg)
          ptr_2 <- ptrunc(q_hi, "binomial", sz, pb, lower.tail = lt, log.p = lg)
          # because pt will have been rounded
          if (q_trunc[ii] > 0L && lt) {
            expect_gte(pt[ii], ptr_1)
            expect_lte(pt[ii], ptr_2)
          } else if (q_trunc[ii] > 0L && !lt) {
            expect_lte(pt[ii], ptr_1)
            expect_gte(pt[ii], ptr_2)
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
        q_trunc <- qtrunc(pt, fam, df, lower.tail = lt, log.p = lg)
        q_stats <- qchisq(pt, df, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_equal(q_trunc[ii], q_stats[ii])
          # Working back to p from q
          ptr <- ptrunc(q_trunc[ii], fam, df, lower.tail = lt, log.p = lg)
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (contbern)", {
  fam <- "contbern"
  for (i in seq_len(3L)) {
    lambda <- runif(1)
    pt <- runif(i)
    q_trunc <- qtrunc(pt, fam, lambda)
    q_stats <- qcontbern(pt, lambda)
    expect_length(pt, i)
    expect_length(q_trunc, i)
    for (ii in seq_along(pt)) {
      expect_equal(q_trunc[ii], q_stats[ii])
      # Working back to p from q
      ptr <- ptrunc(q_trunc[ii], fam, lambda)
      expect_equal(pt[ii], ptr)
    }
  }
})

test_that("qtrunc() works as expected (exp)", {
  fam <- "exp"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        rate <- runif(1L)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        q_trunc <- qtrunc(pt, fam, rate, lower.tail = lt, log.p = lg)
        q_stats <- qexp(pt, rate, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_equal(q_trunc[ii], q_stats[ii])
          # Working back to p from q
          ptr <- ptrunc(q_trunc[ii], fam, rate, lower.tail = lt, log.p = lg)
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (gamma)", {
  fam <- "gamma"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        shp <- rchisq(1L, df = 10L)
        rat <- rchisq(1L, df = 10L)
        skl <- 1 / rat
        pt <- runif(i)
        if (lg) pt <- log(pt)
        q_trunc_sr <- qtrunc(pt, fam, shp, rat, lower.tail = lt, log.p = lg)
        q_trunc_ss <- qtrunc(pt, fam, shp, scale = skl, lower.tail = lt, log.p = lg)
        q_stats <- qgamma(pt, shp, rat, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc_sr, i)
        expect_equal(q_trunc_sr, q_trunc_ss)
        for (ii in seq_along(pt)) {
          expect_equal(q_trunc_sr[ii], q_stats[ii])
          # Working back to p from q
          ptr <- ptrunc(q_trunc_sr[ii], fam, shp, rat, lower.tail = lt, log.p = lg)
          expect_equal(pt[ii], ptr)
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
        if (lg) pt <- log(pt)
        q_trunc <- qtrunc(
          pt, "normal", mean = mn, sd = sg, lower.tail = lt, log.p = lg
        )
        q_norm <- qnorm(pt, mean = mn, sd = sg, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_equal(q_trunc[ii], q_norm[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], "normal", mean = mn, sd = sg,
            lower.tail = lt, log.p = lg
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})
