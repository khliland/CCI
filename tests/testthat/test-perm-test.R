test_that("LogLoss uses the left tail, so dependence gives a small p-value", {
  skip_on_cran()
  set.seed(2)
  n <- 300
  Z <- stats::rnorm(n)
  X <- stats::rnorm(n)
  dat <- data.frame(Z, X, Y = factor(ifelse(Z + 2 * X + stats::rnorm(n, 0, 0.5) > 0, "a", "b")))
  res <- suppressWarnings(CCI.test(Y ~ X | Z, data = dat, metric = "LogLoss", nperm = 20,
                                   nrounds = 100, progress = FALSE, seed = 1))
  expect_equal(res$tail, "left")
  expect_lt(res$p.value, 0.05)
})

test_that("metrics get the correct tail direction", {
  set.seed(1)
  dat <- data.frame(Z = stats::rnorm(100), X = stats::rnorm(100))
  dat$Y <- dat$Z + stats::rnorm(100)
  res <- CCI.test(Y ~ X | Z, data = dat, nperm = 10, nrounds = 50, progress = FALSE, seed = 1)
  expect_equal(res$tail, "left")
  dat$Y <- factor(dat$Y > 0)
  res <- suppressWarnings(CCI.test(Y ~ X | Z, data = dat, nperm = 10, nrounds = 50, progress = FALSE, seed = 1))
  expect_equal(res$tail, "right")
})
