make_chr_data <- function(n = 300) {
  set.seed(1)
  dat <- data.frame(Z1 = stats::rnorm(n), Z2 = stats::rnorm(n))
  dat$X <- dat$Z1 + stats::rnorm(n)
  dat$Yc <- ifelse(dat$Z2 + dat$X + stats::rnorm(n, 0, 0.5) > 0, "a", "b")
  dat$Y4 <- as.character(cut(dat$Z2 + dat$X, 4, labels = c("q1", "q2", "q3", "q4")))
  dat$Yl <- dat$Yc == "a"
  dat$Zc <- sample(c("u", "v", "w"), n, replace = TRUE)
  dat$Xc <- ifelse(dat$X > 0, "hi", "lo")
  dat
}

test_that("wrapper_ranger and wrapper_svm accept character and logical responses", {
  dat <- make_chr_data()
  expect_gt(wrapper_ranger(Yc ~ X + Z1 + Z2, dat, 1:200, 201:300, metric = "Kappa", num.trees = 100), 0.3)
  expect_lt(wrapper_ranger(Y4 ~ X + Z1 + Z2, dat, 1:200, 201:300, metric = "LogLoss", num.trees = 100), log(4))
  expect_gt(wrapper_ranger(Yl ~ X + Z1 + Z2, dat, 1:200, 201:300, metric = "Kappa", num.trees = 100), 0.3)
  expect_gt(wrapper_ranger(Yc ~ Xc + Zc + Z2, dat, 1:200, 201:300, metric = "Kappa", num.trees = 100), 0.2)
  acc <- function(actual, predictions) mean(as.character(actual) == as.character(predictions))
  expect_gt(wrapper_ranger(Yc ~ X + Z1 + Z2, dat, 1:200, 201:300, metric = "acc", metricfunc = acc,
                           num.trees = 100), 0.6)
  expect_gt(wrapper_svm(Yc ~ X + Z1 + Z2, dat, 1:200, 201:300, metric = "acc", metricfunc = acc), 0.6)
})

test_that("CCI.test handles character and logical variables with every method", {
  skip_on_cran()
  dat <- make_chr_data()
  for (m in c("rf", "xgboost", "svm", "KNN")) {
    res <- CCI.test(Yc ~ X | Z1 + Z2, dat, method = m, nperm = 10, nrounds = 50, progress = FALSE, seed = 1)
    expect_equal(res$metric, "Kappa")
    expect_false(anyNA(unlist(res$null.distribution)))
    expect_lt(res$p.value, 0.2)
  }
  res <- CCI.test(Yl ~ X | Z1 + Z2, dat, nperm = 10, nrounds = 50, progress = FALSE, seed = 1)
  expect_equal(res$metric, "Kappa")
  expect_lt(res$p.value, 0.2)
  res <- CCI.test(Y4 ~ X | Z1 + Z2, dat, metric = "LogLoss", nperm = 10, nrounds = 50, progress = FALSE, seed = 1)
  expect_lt(res$p.value, 0.2)
})

test_that("character X and Z are treated as categorical", {
  skip_on_cran()
  dat <- make_chr_data()
  res <- CCI.test(Yc ~ X | Z1 + Zc, dat, nperm = 10, nrounds = 50, progress = FALSE, seed = 1)
  expect_false(anyNA(unlist(res$null.distribution)))
  # Polynomial terms for the numeric Z1, but not for the categorical Zc
  terms <- all.vars(res$ext_formula)
  expect_true(all(c("Z1_d_2", "Z1_d_3") %in% terms))
  expect_false(any(grepl("^Zc_d_", terms)))
  res <- CCI.test(Yc ~ Xc | Z1 + Z2, dat, method = "xgboost", nperm = 10, nrounds = 50, progress = FALSE, seed = 1)
  expect_false(anyNA(unlist(res$null.distribution)))
})

test_that("tuning works with a character response", {
  skip_on_cran()
  dat <- make_chr_data()
  tuned <- CCI.pretuner(Yc ~ X | Z1 + Z2, dat, metric = "Kappa", samples = 2, folds = 2, progress = FALSE)
  expect_false(anyNA(tuned$tuning_result$Kappa))
  res <- CCI.test(Yc ~ X | Z1 + Z2, dat, tune = TRUE, samples = 2, folds = 2, nperm = 10, progress = FALSE, seed = 1)
  expect_lt(res$p.value, 0.2)
})
