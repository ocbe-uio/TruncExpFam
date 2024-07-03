context("qtrunc, upper truncation")

test_that("qtrunc() works as expected (beta)", {
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        b <- qtrunc(
          max(runif(10L, pt)), "beta", shp1, shp2, lower.tail = lt, log.p = FALSE
        )
        q_trunc <- qtrunc(
          pt, "beta", shp1, shp2, b = b, lower.tail = lt, log.p = lg
        )
        q_stats <- qbeta(pt, shp1, shp2, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_lt(q_trunc[ii], q_stats[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], "beta", shp1, shp2, lower.tail = lt, log.p = lg, b = b
          )
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
        b <- qtrunc(
          max(runif(10L, pt)), "normal", mean = mn, sd = sg,
          lower.tail = lt, log.p = FALSE
        )
        if (lg) pt <- log(pt)
        q_trunc <- qtrunc(
          pt, "normal", mean = mn, sd = sg, b = b, lower.tail = lt, log.p = lg
        )
        q_norm <- qnorm(pt, mean = mn, sd = sg, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_lt(q_trunc[ii], q_norm[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], "normal", mean = mn, sd = sg, b = b,
            lower.tail = lt, log.p = lg
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})
