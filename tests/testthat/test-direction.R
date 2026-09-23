test_that("CCI.direction does not depend on the units of Y and X", {
  skip_on_cran()
  set.seed(1)
  n <- 300
  dat <- data.frame(Z1 = stats::rnorm(n), Z2 = stats::rnorm(n))
  dat$X <- dat$Z1 + stats::rnorm(n)
  dat$Y <- dat$Z2 + stats::rnorm(n)
  direction <- function(d, seed = 1) {
    set.seed(seed)
    deparse(CCI.direction(Y ~ X | Z1 + Z2, d, method = "xgboost", nrounds = 30))
  }
  base <- direction(dat)
  for (scale_Y in c(0.01, 100)) {
    d2 <- dat
    d2$Y <- d2$Y * scale_Y + 5
    expect_equal(direction(d2), base)
  }
  d3 <- dat
  d3$X <- d3$X * 100
  expect_equal(direction(d3), base)
})

test_that("CCI.direction picks the variable that is easier to predict", {
  skip_on_cran()
  set.seed(2)
  n <- 400
  dat <- data.frame(Z1 = stats::rnorm(n), Z2 = stats::rnorm(n))
  dat$X <- dat$Z1 + dat$Z2 + stats::rnorm(n, 0, 0.1)     # X is almost fully explained by Z
  dat$Y <- stats::rnorm(n) * 1000                        # Y is noise, on a much larger scale
  f <- CCI.direction(Y ~ X | Z1 + Z2, dat, method = "xgboost", nrounds = 30)
  expect_equal(deparse(f), "X ~ Y | Z1 + Z2")
})

test_that("CCI.direction stops for a constant variable", {
  dat <- data.frame(Z = stats::rnorm(20), X = stats::rnorm(20), Y = 1)
  expect_error(CCI.direction(Y ~ X | Z, dat, method = "xgboost", nrounds = 5), "zero variance")
})

test_that("CCI.direction uses the package's own learners, not caret", {
  skip_on_cran()
  set.seed(3)
  n <- 200
  dat <- data.frame(Z1 = stats::rnorm(n), Z2 = stats::rnorm(n))
  dat$X <- dat$Z1 + dat$Z2 + stats::rnorm(n, 0, 0.1)
  dat$Y <- dat$Z1 + stats::rnorm(n)
  local_mocked_bindings(train = function(...) stop("caret::train should not be used"), .package = "caret")
  for (m in c("rf", "xgboost", "svm", "KNN")) {
    f <- CCI.direction(Y ~ X | Z1 + Z2, dat, method = m, nrounds = 50)
    expect_equal(deparse(f), "X ~ Y | Z1 + Z2")
  }
  expect_equal(deparse(CCI.direction(Y ~ X | 1, dat, method = "KNN")), "X ~ Y | 1")
  expect_error(CCI.direction(Y ~ X | Z1, dat, method = "lm"), "method must be")
})
