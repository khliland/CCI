# Test script for the CCI package
devtools::load_all()
library(CCI)
set.seed(123)
# Testing utils functions
# Clean formula
#-------------------------------------------------------------------------------
test_that("clean_formula outputs correct formula", {
  clean_formula <- clean_formula(y ~ x | z)
  expect_true(class(clean_formula) == "formula")
  expect_equal(clean_formula, y ~ x | z)
})
#-------------------------------------------------------------------------------
test_that("clean_formula outputs correct formula", {
  clean_formula <- clean_formula(y ~ x + z)
  expect_true(class(clean_formula) == "formula")
  expect_equal(clean_formula, y ~ x | z)
})
#-------------------------------------------------------------------------------
# Get pvalues
dist <- rnorm(10)
test_statistic <- rnorm(1)
test_that("get_pvalues outputs p-values", {
  p_value <- get_pvalues(dist = dist, test_statistic = test_statistic, tail = "right")
  expect_lt(p_value,1)
  expect_gt(p_value, 0)
})
#-------------------------------------------------------------------------------
test_that("get_pvalues outputs p-values", {
  p_value <- get_pvalues(dist = dist, test_statistic = test_statistic, parametric = TRUE, tail = "right")
  expect_lt(p_value,1)
  expect_gt(p_value, 0)
})
#-------------------------------------------------------------------------------
# Testing wrapper functions
dat <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  x3 = rnorm(100),
  x4 = rnorm(100),
  y = rnorm(100)
)
train_indices <- c(1:80)
test_indices <- c(81:100)
#-------------------------------------------------------------------------------
test_that("glm_wrapper outputs a metric score (basic use)", {
  metric <- glm_wrapper(formula = y ~ x1 + x2 + x3 + x4, data = dat, train_indices = train_indices, test_indices = test_indices, data_type = "continuous", family = gaussian(link = "identity"))
  expect_true(class(metric) == "numeric")
})

normal_data <- function(N){
  Z1 <- rnorm(N,0,1)
  Z2 <- rnorm(N,0,1)
  X <- rnorm(N, Z1 + Z2, 1)
  Y <- rnorm(N, Z1 + Z2, 1)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}

metricfunc <- function(data, model, test_indices) {
  actual <- data[test_indices,][['Y']]
  pred <- predict.glm(model, newdata = data[test_indices,])
  sst <- sum((actual - mean(actual))^2)
  ssr <- sum((actual - pred)^2)
  metric <- 1 - (ssr / sst)
  return(metric)
}

test_that("glm_wrapper outputs a custom metric score (advance use)", {
  dat <- normal_data(200)
  inTraining <- sample(1:nrow(dat), size = floor(0.8 * nrow(dat)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(dat), inTraining)
  metric <- glm_wrapper(formula = Y ~ X + Z1 + Z2,
                        data = dat,
                        train_indices = train_indices,
                        test_indices = test_indices,
                        data_type = "continuous",
                        family = gaussian(link = "identity"),
                        metricfunc = metricfunc)
  expect_true(class(metric) == "numeric")
})
#-------------------------------------------------------------------------------

test_that("glm_wrapper outputs a metric score (binary var)", {
  dat <- binomial_data(300, 1, 1)
  inTraining <- sample(1:nrow(dat), size = floor(0.8 * nrow(dat)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(dat), inTraining)
  metric <- glm_wrapper(formula = Y ~ X + Z1 + Z2,
                        data = dat,
                        train_indices = train_indices,
                        test_indices = test_indices,
                        data_type = "binary",
                        family = binomial(link = "logit"))
  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

multi_class_log_loss <- function(data, model, all_levels, eps = 0.001) {
  pred <- predict(model, newdata = data[test_indices,], type = "probs")
  actual <- data[test_indices,][[all.vars(formula)[1]]]
  actual <- factor(actual, levels = all_levels)
  actual_matrix <- model.matrix(~ actual - 1)
  predicted <- pmax(pmin(pred, 1 - eps), eps)
  log_loss <- -sum(actual_matrix * log(predicted)) / nrow(predicted)
  return(log_loss)
}



test_that("multinom_wrapper outputs a metric score", {
  dat <- CategorizeInteractiondData(200)
  inTraining <- sample(1:nrow(dat), size = floor(0.8 * nrow(dat)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(dat), inTraining)
  metric <- multinom_wrapper(formula = Y ~ X + Z1 + Z2,
                        data = dat,
                        train_indices = train_indices,
                        test_indices = test_indices,
                        data_type = "categorical",
                        metricfunc = metricfunc
                        )
  expect_true(class(metric) == "numeric")
})



check_formula(formula = y ~ x | z, data = dat)
check_formula(formula = y ~ x | z, data = dat)

non_lin_normal <- function(N){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = exp(Z1*Z2) + rnorm(N,0,1)
  Y <- Z1*Z2 + rnorm(N,0,1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}
dat <- non_lin_normal(800)
test_that("perm.test works correctly for continuous data", {
  result <- perm.test(y ~ x1 | x2 + x3 + x4, data = dat, method = "lm", nperm = 150, parametric = FALSE)
  expect_is(result, "CCI")
})

test_that("perm.test works correctly for continuous data", {
  result <- perm.test(y ~ x1 | x2 + x3 + x4, data = dat, method = "rf", nperm = 150, parametric = FALSE)
  expect_is(result, "CCI")
})

test_that("perm.test works correctly for continuous data", {
  result <- perm.test(y ~ x1 | x2 + x3 + x4, data = dat, method = "xgboost", nperm = 50, parametric = FALSE)
  expect_is(result, "CCI")
})

expect_true("distribution" %in% names(result))
  expect_true("test.statistic" %in% names(result))
  expect_true("p.value" %in% names(result))

?expect_is()

test <- perm.test(formula = y ~ x1 | x2 + x3 + x4, data = dat)
summary(test)
plot(test, title = "Test")

CCI.test(formula = y ~ x1 | x2 + x3 + x4, data = dat, nperm = 100, method = "lm")

dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
cci <- perm.test("y ~ x1 | x2", data = dat)
QQplot(cci)
dag <- dagitty('dag {
  X -> Y
  Y -> Z

}')
plot(dag)
t <- impliedConditionalIndependencies(dag)


result <- perm.test(formula = "Y ~ X | Z1", data = data, method = "rf", nperm = 1000, parametric = TRUE)
summary_result <- summary(result)
print(summary_result)
