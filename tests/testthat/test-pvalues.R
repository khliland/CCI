test_that("get_pvalues removes missing null values with a warning", {
  null <- c(1:9, NA)
  expect_warning(p <- get_pvalues(null, 0, tail = "left"), "1 of 10 values")
  expect_equal(p, 1 / 10)
  expect_warning(p <- get_pvalues(null, 5, tail = "right"), "remaining 9")
  expect_equal(p, (5 + 1) / 10)
  expect_warning(p <- get_pvalues(c(NaN, stats::rnorm(20)), 0, parametric = TRUE), "1 of 21")
  expect_true(is.finite(p))
})

test_that("get_pvalues gives unchanged results without missing values", {
  expect_no_warning(p <- get_pvalues(1:9, 0, tail = "left"))
  expect_equal(p, 1 / 10)
  expect_equal(get_pvalues(1:9, 10, tail = "right"), 1 / 10)
})

test_that("get_pvalues returns NA with a warning when nothing can be computed", {
  expect_warning(p <- get_pvalues(1:9, NA, tail = "left"), "test statistic is missing")
  expect_true(is.na(p))
  expect_warning(expect_warning(p <- get_pvalues(c(NA, NA, 1), 0), "Fewer than two"), "2 of 3")
  expect_true(is.na(p))
})
