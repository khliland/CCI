test_that("clean_formula handles conditional formulas as before", {
  expect_equal(clean_formula(y ~ x | z + v), y ~ x | z + v, ignore_formula_env = TRUE)
  expect_equal(clean_formula(y ~ x + z + v), y ~ x | z + v, ignore_formula_env = TRUE)
})

test_that("unconditional tests are written Y ~ X | 1 or Y ~ X + 1", {
  expect_equal(clean_formula(y ~ x | 1), y ~ x | 1, ignore_formula_env = TRUE)
  expect_equal(clean_formula(y ~ x + 1), y ~ x | 1, ignore_formula_env = TRUE)
  expect_equal(clean_formula(y ~ 1 + x), y ~ x | 1, ignore_formula_env = TRUE)
})

test_that("formulas without conditioning variables and without 1 give an informative error", {
  expect_error(clean_formula(y ~ x), "Y ~ X | 1", fixed = TRUE)
  expect_error(clean_formula(y ~ x | 0), "Y ~ X | 1", fixed = TRUE)
  expect_error(clean_formula(y ~ 1), "Y ~ X | 1", fixed = TRUE)
  expect_error(CCI.test(y ~ x, data.frame(x = 1:10, y = 1:10)), "unconditional testing")
})

test_that("build_formula keeps the explicit 1 for unconditional tests", {
  expect_equal(build_formula(y ~ x | 1), y ~ x + 1, ignore_formula_env = TRUE)
  expect_equal(clean_formula(build_formula(y ~ x | 1)), y ~ x | 1, ignore_formula_env = TRUE)
  expect_equal(build_formula(y ~ x | z, "z_d_2"), y ~ x + z + z_d_2, ignore_formula_env = TRUE)
})

test_that("unconditional CCI.test works for all methods and detects dependence", {
  skip_on_cran()
  set.seed(1)
  n <- 300
  dat <- data.frame(X = stats::rnorm(n))
  dat$Y <- dat$X + stats::rnorm(n, 0, 0.5)
  dat$Y0 <- stats::rnorm(n)
  for (m in c("rf", "xgboost", "svm", "KNN")) {
    res <- CCI.test(Y ~ X | 1, dat, method = m, nperm = 20, nrounds = 50, progress = FALSE, seed = 1)
    expect_lt(res$p.value, 0.1)
    expect_false(anyNA(unlist(res$null.distribution)))
  }
  res0 <- CCI.test(Y0 ~ X + 1, dat, nperm = 20, nrounds = 50, progress = FALSE, seed = 1)
  expect_gt(res0$p.value, 0.1)
  expect_equal(res0$formula, Y0 ~ X + 1, ignore_formula_env = TRUE)
})

test_that("unconditional CCI.test works with categorical Y, tuning and direction", {
  skip_on_cran()
  set.seed(2)
  n <- 300
  dat <- data.frame(X = stats::rnorm(n))
  dat$Y <- dat$X + stats::rnorm(n, 0, 0.5)
  dat$C <- factor(ifelse(dat$Y > 0, "yes", "no"))
  res <- CCI.test(C ~ X | 1, dat, nperm = 20, nrounds = 50, progress = FALSE, seed = 1)
  expect_lt(res$p.value, 0.1)
  res <- CCI.test(Y ~ X | 1, dat, nperm = 20, tune = TRUE, samples = 2, folds = 2, progress = FALSE, seed = 1)
  expect_lt(res$p.value, 0.1)
  direction <- deparse(CCI.direction(Y ~ X | 1, dat, method = "xgboost", nrounds = 20))
  expect_true(direction %in% c("Y ~ X | 1", "X ~ Y | 1"))
})
