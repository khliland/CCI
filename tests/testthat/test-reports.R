make_report_data <- function(n = 200) {
  set.seed(1)
  dat <- data.frame(Z1 = stats::rnorm(n), Z2 = stats::rnorm(n))
  dat$X <- dat$Z1 + dat$Z2 + stats::rnorm(n, 0, 0.1)
  dat$Y <- dat$Z1 + stats::rnorm(n)
  dat
}

test_that("print.CCI gives a short overview", {
  dat <- make_report_data()
  res <- CCI.test(Y ~ X | Z1 + Z2, dat, nperm = 10, nrounds = 30, progress = FALSE, seed = 1)
  out <- capture.output(print(res))
  expect_true(any(grepl("Formula:   Y ~ X | Z1 + Z2", out, fixed = TRUE)))
  expect_true(any(grepl("rf with metric RMSE and 10 permutations", out, fixed = TRUE)))
  expect_true(any(grepl("P-value:", out, fixed = TRUE)))
  expect_false(any(grepl("direction chosen", out)))
  capture.output(visible <- withVisible(print(res))$visible)
  expect_false(visible)
  expect_output(print(CCI.test(Y ~ X | Z1, dat, parametric = TRUE, nperm = 10, nrounds = 30,
                               progress = FALSE, seed = 1)), "(parametric)", fixed = TRUE)
})

test_that("print and summary show the tested direction", {
  skip_on_cran()
  dat <- make_report_data()
  res <- CCI.test(Y ~ X | Z1 + Z2, dat, method = "xgboost", nrounds = 30, choose_direction = TRUE,
                  nperm = 10, progress = FALSE, seed = 1)
  expect_equal(deparse(res$tested_formula), "X ~ Y | Z1 + Z2")
  expect_output(print(res), "Formula:   X ~ Y | Z1 + Z2", fixed = TRUE)
  expect_output(print(res), "given as Y ~ X | Z1 + Z2", fixed = TRUE)
  expect_output(print(summary(res)), "Direction: chosen by choose_direction; given as Y ~ X | Z1 + Z2",
                fixed = TRUE)
})

test_that("summary works for objects from perm.test", {
  dat <- make_report_data()
  res <- perm.test(Y ~ X | Z1 + Z2, dat, nperm = 10, nrounds = 30, progress = FALSE)
  expect_output(print(summary(res)), "Formula:   Y ~ X | Z1 + Z2", fixed = TRUE)
  expect_output(print(res), "Statistic:")
})
