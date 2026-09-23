make_mc_data <- function(n = 1000) {
  set.seed(1)
  dat <- data.frame(Z = stats::rnorm(n), X = stats::rnorm(n))
  dat$Y <- dat$Z + stats::rnorm(n)
  dat
}

test_that("MC_sample sets the share of data used in each Monte Carlo sample", {
  dat <- make_mc_data()
  res <- CCI.test(Y ~ X | Z, dat, method = "KNN", nperm = 10, progress = FALSE, seed = 1)
  expect_equal(res$MC_sample, (900 / 1000)^0.75)                     # "Auto" with n > 900
  res <- CCI.test(Y ~ X | Z, dat, method = "KNN", MC_sample = "No", nperm = 10, progress = FALSE, seed = 1)
  expect_equal(res$MC_sample, 1)
  res <- CCI.test(Y ~ X | Z, dat, method = "KNN", MC_sample = "Yes", MC_sample_set = 0.4,
                  nperm = 10, progress = FALSE, seed = 1)
  expect_equal(res$MC_sample, 0.4)
  expect_equal(res$settings$MC_sample, 0.4)
  expect_output(print(summary(res)), "MC sample:   0.4", fixed = TRUE)
})

test_that("MC_sample gives informative errors", {
  dat <- make_mc_data(100)
  expect_error(CCI.test(Y ~ X | Z, dat, MC_sample = "Yes"), "MC_sample_set")
  expect_error(CCI.test(Y ~ X | Z, dat, MC_sample = "Yes", MC_sample_set = 2), "MC_sample_set")
  expect_error(CCI.test(Y ~ X | Z, dat, MC_sample = "Maybe"), "Invalid MC_sample")
})

test_that("the old names subsample and subsample_set still work, with a warning", {
  dat <- make_mc_data()
  warnings <- capture_warnings(res <- CCI.test(Y ~ X | Z, dat, method = "KNN", subsample = "Yes",
                                               subsample_set = 0.4, nperm = 10, progress = FALSE, seed = 1))
  expect_match(warnings, "'subsample' is deprecated; use 'MC_sample'", fixed = TRUE, all = FALSE)
  expect_match(warnings, "'subsample_set' is deprecated; use 'MC_sample_set'", fixed = TRUE, all = FALSE)
  expect_equal(res$MC_sample, 0.4)
  expect_warning(res <- perm.test(Y ~ X | Z, dat, method = "KNN", subsample = 0.5, nperm = 10,
                                  progress = FALSE), "use 'MC_sample'")
  expect_equal(res$MC_sample, 0.5)
  expect_warning(test.gen(Y ~ X | Z, dat, method = "KNN", subsample = 0.5, nperm = 10,
                          permutation = TRUE, progress = FALSE), "deprecated")
  expect_warning(CCI.direction(Y ~ X | Z, dat, method = "KNN", subsample = 0.5), "deprecated")
})

test_that("summary and QQplot work for objects that store the old field name", {
  dat <- make_mc_data(200)
  res <- CCI.test(Y ~ X | Z, dat, method = "KNN", nperm = 10, progress = FALSE, seed = 1)
  old <- res
  old$subsample <- old$MC_sample
  old$MC_sample <- NULL
  old$settings <- NULL
  expect_output(print(summary(old)), "MC sample:   1", fixed = TRUE)
  expect_false(anyNA(QQplot(old, progress = FALSE)$data$pvalues))
})
