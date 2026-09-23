make_qq_data <- function(n = 200) {
  set.seed(1)
  dat <- data.frame(Z1 = stats::rnorm(n), Z2 = stats::rnorm(n))
  dat$X <- dat$Z1 + stats::rnorm(n)
  dat$Y <- dat$Z2 + stats::rnorm(n)
  dat
}
qq_pvalues <- function(res, ...) QQplot(res, progress = FALSE, ...)$data$pvalues

test_that("QQplot works for all built-in methods", {
  skip_on_cran()
  dat <- make_qq_data()
  for (m in c("rf", "xgboost", "svm", "KNN")) {
    res <- CCI.test(Y ~ X | Z1 + Z2, dat, method = m, nperm = 10, nrounds = 30, progress = FALSE, seed = 1)
    p <- qq_pvalues(res)
    expect_length(p, 10)
    expect_false(anyNA(p))
  }
})

test_that("QQplot works with a custom metricfunc and mlfunc", {
  skip_on_cran()
  dat <- make_qq_data()
  r2 <- function(actual, predictions, ...) 1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
  res <- CCI.test(Y ~ X | Z1 + Z2, dat, metricfunc = r2, tail = "right", nperm = 10, nrounds = 30,
                  progress = FALSE, seed = 1)
  expect_false(anyNA(qq_pvalues(res)))

  lm_rmse <- function(formula, data, train_indices, test_indices, ...) {
    fit <- stats::lm(formula, data = data[train_indices, ])
    y <- data[test_indices, all.vars(formula)[1]]
    sqrt(mean((stats::predict(fit, data[test_indices, ]) - y)^2))
  }
  res <- CCI.test(Y ~ X | Z1 + Z2, dat, mlfunc = lm_rmse, tail = "left", nperm = 10,
                  progress = FALSE, seed = 1)
  expect_false(anyNA(qq_pvalues(res)))
})

test_that("QQplot repeats the test with the stored model settings", {
  skip_on_cran()
  dat <- make_qq_data()
  res <- CCI.test(Y ~ X | Z1 + Z2, dat, method = "xgboost", eta = 0.05, max_depth = 2, nrounds = 30,
                  nperm = 10, progress = FALSE, seed = 1)
  used <- list()
  local_mocked_bindings(
    wrapper_xgboost = function(formula, data, train_indices, test_indices, metric, nrounds = 500,
                               metricfunc = NULL, nthread = 1, eps = 1e-15, MC_sample = 1, ...) {
      used[[length(used) + 1]] <<- c(list(nrounds = nrounds), list(...))
      stats::runif(1)
    }
  )
  qq_pvalues(res)
  expect_length(used, 10)
  expect_true(all(vapply(used, function(u) u$eta == 0.05 && u$max_depth == 2 && u$nrounds == 30, logical(1))))
})

test_that("QQplot arguments override the stored settings", {
  skip_on_cran()
  dat <- make_qq_data()
  res <- CCI.test(Y ~ X | Z1 + Z2, dat, nperm = 10, nrounds = 30, progress = FALSE, seed = 1)
  expect_length(qq_pvalues(res, nperm = 5), 5)
})

test_that("QQplot works for objects from perm.test and older objects without settings", {
  skip_on_cran()
  dat <- make_qq_data()
  res <- perm.test(Y ~ X | Z1 + Z2, dat, nperm = 10, nrounds = 30, progress = FALSE)
  expect_false(anyNA(qq_pvalues(res)))
  res$settings <- NULL
  expect_false(anyNA(qq_pvalues(res)))
  res$metric <- "r2"
  expect_error(qq_pvalues(res), "older version")
})
