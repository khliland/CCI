make_data <- function(n = 150) {
  set.seed(1)
  Z1 <- stats::rnorm(n)
  Z2 <- stats::rnorm(n)
  X <- Z1 + stats::rnorm(n)
  Y <- Z1 + Z2 + stats::rnorm(n)
  data.frame(Z1, Z2, X, Y)
}

test_that("pretuner returns sorted results and best parameters for rf", {
  dat <- make_data()
  res <- CCI.pretuner(Y ~ X | Z1 + Z2, data = dat, method = "rf",
                      samples = 3, folds = 2, progress = FALSE)
  expect_named(res, c("best_param", "tuning_result", "warnings"))
  expect_equal(res$best_param$method, "rf")
  expect_equal(res$best_param$RMSE, min(res$tuning_result$RMSE))
  expect_false(is.unsorted(res$tuning_result$RMSE))
  expect_named(get_tuned_params(res$best_param), "mtry")
})

test_that("pretuner respects user supplied mtry", {
  dat <- make_data()
  res <- CCI.pretuner(Y ~ X | Z1 + Z2, data = dat, method = "rf", mtry = 2,
                      random_grid = FALSE, folds = 2, progress = FALSE)
  expect_equal(unique(res$tuning_result$mtry), 2)
})

test_that("pretuner supports classification metrics", {
  dat <- make_data()
  dat$Y <- factor(ifelse(dat$Y > 0, "high", "low"))
  res <- CCI.pretuner(Y ~ X | Z1 + Z2, data = dat, method = "rf", metric = "Kappa",
                      samples = 2, folds = 2, progress = FALSE)
  expect_equal(res$best_param$Kappa, max(res$tuning_result$Kappa))
})

test_that("pretuner does not expand already expanded terms", {
  dat <- make_data()
  res <- CCI.pretuner(Y ~ X | Z1 + Z2, data = dat, method = "rf", poly = FALSE,
                      interaction = FALSE, random_grid = FALSE, folds = 2, progress = FALSE)
  # With only Z1 and Z2 as predictors, mtry can not exceed 2
  expect_true(all(res$tuning_result$mtry <= 2))
})

test_that("pretuner gives informative errors", {
  dat <- make_data()
  expect_error(CCI.pretuner(Y ~ X | Z1, data = dat, metric = "MAE"), "metric must be")
  expect_error(CCI.pretuner(Y ~ X | Z1, data = dat, method = "KNN"), "method must be")
  expect_error(CCI.pretuner(Y ~ X | Z1, data = dat, folds = 1), "folds")
})

test_that("tuned parameters are passed on to the model in CCI.test", {
  skip_on_cran()
  dat <- make_data()
  used_mtry <- NULL
  local_mocked_bindings(
    wrapper_ranger = function(formula, data, train_indices, test_indices, metric,
                              metricfunc = NULL, nthread = 1, mtry = NULL, num.trees, ...) {
      used_mtry <<- c(used_mtry, mtry)
      stats::runif(1)
    }
  )
  res <- CCI.test(Y ~ X | Z1 + Z2, data = dat, nperm = 10, tune = TRUE, samples = 2,
                  folds = 2, poly = FALSE, interaction = FALSE, progress = FALSE, seed = 1)
  expect_false(is.null(used_mtry))
  # The last nperm + 1 calls are the test itself and must all use the tuned mtry
  expect_true(all(utils::tail(used_mtry, 11) == utils::tail(used_mtry, 1)))
})

test_that("user given xgboost parameters reach the model in CCI.test", {
  skip_on_cran()
  dat <- make_data()
  used_eta <- NULL
  local_mocked_bindings(
    wrapper_xgboost = function(formula, data, train_indices, test_indices, metric,
                               metricfunc = NULL, nrounds = 500, nthread = 1, subsample = 1, ...) {
      used_eta <<- c(used_eta, list(...)$eta)
      stats::runif(1)
    }
  )
  CCI.test(Y ~ X | Z1 + Z2, data = dat, nperm = 10, method = "xgboost", eta = 0.123,
           progress = FALSE, seed = 1)
  expect_true(length(used_eta) > 0 && all(used_eta == 0.123))
})

test_that("pretuner works for xgboost and svm", {
  skip_on_cran()
  dat <- make_data()
  res_xgb <- CCI.pretuner(Y ~ X | Z1 + Z2, data = dat, method = "xgboost", nrounds = c(20, 50),
                          samples = 2, folds = 2, progress = FALSE)
  expect_false(anyNA(res_xgb$best_param$RMSE))
  expect_setequal(names(get_tuned_params(res_xgb$best_param)),
                  c("eta", "max_depth", "gamma", "colsample_bytree", "min_child_weight", "nrounds"))

  res_svm <- CCI.pretuner(Y ~ X | Z1 + Z2, data = dat, method = "svm",
                          samples = 2, folds = 2, progress = FALSE)
  expect_false(anyNA(res_svm$best_param$RMSE))
  expect_named(get_tuned_params(res_svm$best_param), c("gamma", "cost"))
})

test_that("wrapper_ranger computes Kappa for binary outcomes with any labels", {
  dat <- make_data()
  dat$Y <- factor(ifelse(dat$Y > 0, "high", "low"))
  kappa <- wrapper_ranger(Y ~ Z1 + Z2, data = dat, train_indices = 1:100, test_indices = 101:150,
                          metric = "Kappa", num.trees = 100)
  expect_true(is.finite(kappa) && kappa > 0)

  dat$Y01 <- as.numeric(dat$Y == "high")
  kappa01 <- wrapper_ranger(Y01 ~ Z1 + Z2, data = dat, train_indices = 1:100, test_indices = 101:150,
                            metric = "Kappa", num.trees = 100)
  expect_true(is.finite(kappa01) && kappa01 > 0)
})
