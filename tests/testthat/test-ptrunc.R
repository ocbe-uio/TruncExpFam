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
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_norm[q])
        }
      }
    }
  }
})
