make_xgb_data <- function(n = 200) {
  set.seed(1)
  Z1 <- stats::rnorm(n)
  Z2 <- stats::rnorm(n)
  Y <- Z1 + Z2 + stats::rnorm(n, 0, 0.5)
  data.frame(Z1, Z2, Y)
}
xgb <- function(formula, data, metric, ...) {
  wrapper_xgboost(formula, data, train_indices = 1:120, test_indices = 121:200,
                  metric = metric, nrounds = 30, ...)
}

test_that("regression gives RMSE, and a custom metric gets numeric actual values", {
  dat <- make_xgb_data()
  expect_true(xgb(Y ~ Z1 + Z2, dat, "RMSE") < stats::sd(dat$Y))
  r2 <- function(actual, predictions) {
    stopifnot(is.numeric(actual), is.numeric(predictions))
    1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
  }
  expect_gt(xgb(Y ~ Z1 + Z2, dat, "r2", metricfunc = r2), 0.5)
})

test_that("custom metric without ... works when model parameters are passed", {
  dat <- make_xgb_data()
  r2 <- function(actual, predictions) 1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
  expect_gt(xgb(Y ~ Z1 + Z2, dat, "r2", metricfunc = r2, eta = 0.1, max_depth = 3), 0.5)
})

test_that("binary classification works for any class labels", {
  dat <- make_xgb_data()
  labels <- list(zero_one = as.numeric(dat$Y > 0), one_two = ifelse(dat$Y > 0, 2, 1),
                 factor = factor(ifelse(dat$Y > 0, "high", "low")), logical = dat$Y > 0)
  for (nm in names(labels)) {
    dat$C <- labels[[nm]]
    expect_gt(xgb(C ~ Z1 + Z2, dat, "Kappa"), 0.3)
    expect_lt(xgb(C ~ Z1 + Z2, dat, "LogLoss"), log(2))
  }
})

test_that("multiclass classification works for numeric labels 1..K and factors", {
  dat <- make_xgb_data()
  dat$C <- as.integer(cut(dat$Y, 3))            # 1, 2, 3
  expect_gt(xgb(C ~ Z1 + Z2, dat, "Kappa"), 0.3)
  expect_lt(xgb(C ~ Z1 + Z2, dat, "LogLoss"), log(3))
  dat$C <- factor(dat$C, labels = c("low", "mid", "high"))
  expect_gt(xgb(C ~ Z1 + Z2, dat, "Kappa"), 0.3)
})

test_that("custom metric for classification gets a factor and class probabilities", {
  dat <- make_xgb_data()
  dat$C <- factor(cut(dat$Y, 3, labels = c("a", "b", "c")))
  acc <- function(actual, predictions) {
    stopifnot(is.factor(actual), is.matrix(predictions), identical(colnames(predictions), levels(actual)))
    mean(colnames(predictions)[max.col(predictions)] == actual)
  }
  expect_gt(xgb(C ~ Z1 + Z2, dat, "acc", metricfunc = acc), 0.5)
})

test_that("factor predictors with a level missing in the test rows work", {
  dat <- make_xgb_data()
  dat$F <- factor(c(rep("rare", 3), sample(c("a", "b"), 197, replace = TRUE)))
  expect_true(is.finite(xgb(Y ~ Z1 + F, dat, "RMSE")))
})

test_that("CCI.test with xgboost gives p-values for custom metrics and numeric classes", {
  skip_on_cran()
  set.seed(1)
  n <- 300
  dat <- data.frame(Z1 = stats::rnorm(n), Z2 = stats::rnorm(n))
  dat$X <- dat$Z1 + stats::rnorm(n)
  dat$Y <- dat$Z2 + dat$X + stats::rnorm(n)
  r2 <- function(actual, predictions) 1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
  res <- CCI.test(Y ~ X | Z1 + Z2, dat, method = "xgboost", metricfunc = r2, tail = "right",
                  nperm = 10, nrounds = 30, progress = FALSE, seed = 1)
  expect_false(anyNA(unlist(res$null.distribution)))
  expect_lt(res$p.value, 0.2)
  dat$Y <- ifelse(dat$Y > 0, 2, 1)
  res <- CCI.test(Y ~ X | Z1 + Z2, dat, method = "xgboost", metric = "Kappa",
                  nperm = 10, nrounds = 30, progress = FALSE, seed = 1)
  expect_false(anyNA(unlist(res$null.distribution)))
})
