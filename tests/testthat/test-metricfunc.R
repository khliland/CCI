make_mf_data <- function(n = 200) {
  set.seed(1)
  dat <- data.frame(Z1 = stats::rnorm(n), Z2 = stats::rnorm(n))
  dat$X <- dat$Z1 + stats::rnorm(n)
  dat$Y <- dat$Z2 + dat$X + stats::rnorm(n, 0, 0.5)
  dat$C <- factor(ifelse(dat$Y > 0, "high", "low"))
  dat
}
r2 <- function(actual, predictions) {
  stopifnot(is.numeric(actual), is.numeric(predictions))
  1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
}
accuracy <- function(actual, predictions) {
  stopifnot(is.factor(actual), is.factor(predictions))
  mean(as.character(actual) == as.character(predictions))
}

test_that("wrapper_knn accepts a custom metric for continuous and categorical outcomes", {
  dat <- make_mf_data()
  v <- wrapper_knn(Y ~ X + Z1 + Z2, dat, 1:120, 121:200, metric = "r2", metricfunc = r2)
  expect_gt(v, 0.5)
  v <- wrapper_knn(C ~ X + Z1 + Z2, dat, 1:120, 121:200, metric = "accuracy", metricfunc = accuracy)
  expect_gt(v, 0.7)
  expect_error(wrapper_knn(Y ~ X + Z1, dat, 1:120, 121:200, metric = "r2"), "custom metricfunc")
})

test_that("metric functions without ... work in every wrapper", {
  dat <- make_mf_data()
  expect_gt(wrapper_ranger(Y ~ X + Z1 + Z2, dat, 1:120, 121:200, metric = "r2", metricfunc = r2,
                           num.trees = 50, min.node.size = 5), 0.5)
  expect_gt(wrapper_svm(Y ~ X + Z1 + Z2, dat, 1:120, 121:200, metric = "r2", metricfunc = r2,
                        cost = 1), 0.5)
  expect_gt(wrapper_knn(Y ~ X + Z1 + Z2, dat, 1:120, 121:200, metric = "r2", metricfunc = r2,
                        ykernel = NULL), 0.5)
})

test_that("CCI.test and QQplot work with KNN and a custom metric", {
  skip_on_cran()
  dat <- make_mf_data()
  res <- CCI.test(Y ~ X | Z1 + Z2, dat, method = "KNN", metricfunc = r2, tail = "right",
                  nperm = 10, progress = FALSE, seed = 1)
  expect_false(anyNA(unlist(res$null.distribution)))
  expect_lt(res$p.value, 0.2)
  expect_false(anyNA(QQplot(res, progress = FALSE)$data$pvalues))
})
