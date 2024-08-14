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
test_that("glm_wrapper outputs a metric score (basic use)", {
  dat <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100),
    x4 = rnorm(100),
    y = rnorm(100)
  )
  train_indices <- c(1:80)
  test_indices <- c(81:100)
  metric <- glm_wrapper(formula = y ~ x1 + x2 + x3 + x4, data = dat, train_indices = train_indices, test_indices = test_indices, data_type = "continuous", family = gaussian(link = "identity"))
  expect_true(class(metric) == "numeric")
})

rSquared <- function(data, model, test_indices) {
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
                        metricfunc = rSquared)
  expect_true(class(metric) == "numeric")
})
#-------------------------------------------------------------------------------

test_that("glm_wrapper outputs a metric score (binary var)", {
  data <- binomial_data(300, 1, 1)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)
  metric <- glm_wrapper(formula = Y ~ X + Z1 + Z2,
                        data = data,
                        train_indices = train_indices,
                        test_indices = test_indices,
                        data_type = "binary",
                        family = binomial(link = "logit"))
  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------
multi_class_log_loss <- function(data, model, test_indices) {
  eps = 0.001
  pred <- predict(model, newdata = data[test_indices,], type = "probs")
  actual <- data[test_indices,][['Y']]
  actual <- factor(actual, levels = levels(data$Y))
  actual_matrix <- model.matrix(~ actual - 1)
  predicted <- pmax(pmin(pred, 1 - eps), eps)
  log_loss <- -sum(actual_matrix * log(predicted)) / nrow(predicted)
  return(log_loss)
}

test_that("multinom_wrapper outputs a log loss metric score", {
  data <- multinominal_data(1000)
  data$X <- as.factor(data$X)
  data$Y <- as.factor(data$Y)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- multinom_wrapper(formula = Y ~ X + Z1 + Z2,
                        data = data,
                        train_indices = train_indices,
                        test_indices = test_indices,
                        metricfunc = multi_class_log_loss)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("xgboost_wrapper rmse output", {
  set.seed(1)
  data <- uniform_noise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- xgboost_wrapper(formula = Y ~ X + Z1 + Z2,
                             data = data,
                             train_indices = train_indices,
                             test_indices = test_indices,
                             objective = "reg:squarederror",
                             nrounds = 120)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------
test_that("xgboost_wrapper rmse output", {
  set.seed(1)
  data <- uniform_noise(1000)
  data$Y <- abs(data$Y)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- xgboost_wrapper(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            objective = "reg:squaredlogerror",
                            nrounds = 120)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------
test_that("xgboost_wrapper rmse output", {

  data <- uniform_noise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- xgboost_wrapper(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            objective = "reg:pseudohubererror",
                            nrounds = 120)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------
test_that("xgboost_wrapper Kappa score output", {

  data <- binomial_data(300, 1, 1)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- xgboost_wrapper(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            objective = "binary:logistic",
                            nrounds = 120)
  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("xgboost_wrapper Kappa score output", {

  data <- binomial_data(1000, 1, 1)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- xgboost_wrapper(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            objective = "multi:softprob",
                            num_class = 2,
                            nrounds = 120,
                            eta = 0.1,
                            lambda = 0.5,
                            alpha = 0.5)
  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("xgboost_wrapper Kappa score output", {

  data <- simulateComplexCategorization(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- xgboost_wrapper(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            objective = "multi:softprob",
                            num_class = 4,
                            nrounds = 120,
                            eta = 0.1,
                            lambda = 0.5,
                            alpha = 0.5)
  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("xgboost_wrapper Kappa score output", {

  data <- simulateComplexCategorization(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- xgboost_wrapper(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            objective = "multi:softprob",
                            num_class = 4,
                            nrounds = 120,
                            eta = 0.1,
                            lambda = 0.5,
                            alpha = 0.5,
                            metricfunc = multi_class_log_loss)
  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

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
