context("qtrunc, untruncated")

test_that("qtrunc() works as expected (normal)", {
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        q_trunc <- qtrunc(pt, "normal", mean = mn, sd = sg, lower.tail = lt, log.p = lg)
        q_norm <- qnorm(pt, mean = mn, sd = sg, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_equal(q_trunc[ii], q_norm[ii])
          expect_equal(pt[ii], ptrunc(q_trunc[ii], "normal", mean = mn, sd = sg, lower.tail = lt, log.p = lg))
        }
      }
    }
  }
})
