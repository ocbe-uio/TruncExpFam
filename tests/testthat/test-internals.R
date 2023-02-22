context("Internal functions")

test_that("qcontbern works", {
  set.seed(8865323)
  tests <- 10L
  for (t in seq_len(tests)) {
    prob <- runif(1)
    lamb <- runif(1)
    expect_equal(pcontbern(qcontbern(prob, lamb), lamb), prob, tolerance = 1e-3)
  }
})
