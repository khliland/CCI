test_that("add_poly_terms only adds terms for numeric variables", {
  dat <- data.frame(Z1 = stats::rnorm(20), Zf = factor(rep(c("a", "b"), 10)), Z2 = stats::rnorm(20))
  res <- add_poly_terms(dat, c("Z1", "Zf", "Z2"), degree = 3)
  expect_setequal(res$new_terms, c("Z1_d_2", "Z1_d_3", "Z2_d_2", "Z2_d_3"))
  expect_equal(res$data$Z1_d_2, dat$Z1^2)
  expect_length(add_poly_terms(dat, "Zf", degree = 3)$new_terms, 0)
})

test_that("CCI.test keeps polynomial terms for numeric Z when another Z is a factor", {
  skip_on_cran()
  set.seed(1)
  n <- 200
  dat <- data.frame(Z1 = stats::rnorm(n), Zf = factor(sample(c("a", "b"), n, replace = TRUE)))
  dat$X <- dat$Z1^2 + stats::rnorm(n)
  dat$Y <- dat$Z1^2 + stats::rnorm(n)
  res <- CCI.test(Y ~ X | Z1 + Zf, dat, nperm = 10, nrounds = 30, progress = FALSE, seed = 1)
  expect_true("Z1_d_2" %in% all.vars(res$ext_formula))
  expect_false(anyNA(unlist(res$null.distribution)))
})

test_that("wrapper_ranger passes extra arguments to ranger for a continuous outcome", {
  set.seed(1)
  dat <- data.frame(Z = stats::rnorm(200))
  dat$Y <- sin(3 * dat$Z) + stats::rnorm(200, sd = 0.2)
  fit <- function(...) wrapper_ranger(Y ~ Z, dat, 1:150, 151:200, metric = "RMSE", num.trees = 50, ...)
  expect_warning(fit(not_a_ranger_argument = 1), "not_a_ranger_argument")   # reaches ranger
  set.seed(2)
  shallow <- fit(max.depth = 1)
  set.seed(2)
  deep <- fit()
  expect_gt(shallow, deep)       # a forest of stumps predicts sin(3Z) worse
})

test_that("plot.CCI shows a density and adds ggplot2 layers", {
  set.seed(1)
  dat <- data.frame(Z = stats::rnorm(100), X = stats::rnorm(100))
  dat$Y <- dat$Z + stats::rnorm(100)
  res <- CCI.test(Y ~ X | Z, dat, nperm = 10, nrounds = 30, progress = FALSE, seed = 1)
  g <- plot(res)
  expect_equal(g$labels$y, "Density")
  g2 <- plot(res, ggplot2::labs(title = "My test"), ggplot2::theme_bw())
  expect_equal(g2$labels$title, "My test")
  expect_warning(plot(res, "not a layer"), "ignored")
  expect_equal(plot(res, fill_color = "red")$layers[[1]]$aes_params$fill, "red")
})
