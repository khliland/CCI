# Test script for the CCI package
devtools::load_all()
library(CCI)
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
test_that("get_pvalues outputs p-values", {
  dist <- rnorm(10)
  test_statistic <- rnorm(1)
  p_value <- get_pvalues(dist = dist, test_statistic = test_statistic, tail = "right")
  expect_lt(p_value,1)
  expect_gt(p_value, 0)
})

#-------------------------------------------------------------------------------
test_that("get_pvalues outputs p-values", {
  dist <- rnorm(100)
  test_statistic <- rnorm(1)
  p_value <- get_pvalues(dist = dist, test_statistic = test_statistic, parametric = TRUE, tail = "right")
  expect_lt(p_value,1)
  expect_gt(p_value, 0)
})

#-------------------------------------------------------------------------------
# Testing ML-wrapper functions
# The machine learning wrapper functions takes formula, data and indices for training and test data.
# estimates the ML model with training data, and evaluates on testing data. The different ML-wrapper takes
# different parameters depending on the ML model used. The default ML in the test is ranger (random forest).
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

#-------------------------------------------------------------------------------

test_that("glm_wrapper outputs a custom metric score (advance use)", {
  rSquared <- function(data, model, test_indices) {
    actual <- data[test_indices,][['Y']]
    pred <- predict.glm(model, newdata = data[test_indices,])
    sst <- sum((actual - mean(actual))^2)
    ssr <- sum((actual - pred)^2)
    metric <- 1 - (ssr / sst)
    return(metric)
  }

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
  data <- binomial_data(300, 1, 1, 0.5)
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
# Example of a custom function for calculating log loss with categorical outcome

test_that("multinom_wrapper", {
  data <- categorical_data(1000)

  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- multinom_wrapper(formula = Y ~ X + Z1 + Z2,
                             data = data,
                             train_indices = train_indices,
                             test_indices = test_indices)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------
# Example of a custom function for calculating log loss with categorical outcome

test_that("multinom_wrapper outputs a custom log loss metric score", {
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
test_that("xgboost_wrapper rmse output using a different objective", {

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
test_that("xgboost_wrapper rmse output with reg:pseudohubererror and various parameter settings", {

  data <- uniform_noise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- xgboost_wrapper(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            objective = "reg:pseudohubererror",
                            nrounds = 120,
                            eta = 0.1,
                            max_depth = 4,
                            lambda = 0.5)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("xgboost_wrapper rmse output with reg:squarederror, various parameter settings and custom performance metric (R-squared)", {
  rSquared <- function(data, model, test_indices, test_matrix) {
    actual <- data[test_indices,][['Y']]
    pred <- predict(model, newdata = test_matrix)
    sst <- sum((actual - mean(actual))^2)
    ssr <- sum((actual - pred)^2)
    metric <- 1 - (ssr / sst)
    return(metric)
  }

  data <- uniform_noise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- xgboost_wrapper(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            objective = "reg:squarederror",
                            nrounds = 90,
                            eta = 0.1,
                            max_depth = 4,
                            lambda = 0.5,
                            metricfunc = rSquared)

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
                            objective = "binary:logistic",
                            nrounds = 120)
  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("xgboost_wrapper log loss score output", {
  multi_class_log_loss <- function(data, model, test_indices, test_matrix) {
    eps = 0.001
    pred <- predict(model, newdata = test_matrix)
    actual <- data[test_indices,][['Y']]
    actual_matrix <- model.matrix(~ factor(actual) - 1)
    num_classes <- length(unique(actual))
    pred_matrix <- matrix(pred, ncol = num_classes, byrow = TRUE)

    pred_matrix <- pmax(pmin(pred_matrix, 1 - eps), eps)
    log_loss <- -sum(actual_matrix * log(pred_matrix)) / nrow(pred_matrix)
    return(log_loss)
  }

  data <- multinominal_data(1000)
  data$Y <- as.numeric(as.factor(data$Y))-1
  data$X <- as.numeric(as.factor(data$X))-1

  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- xgboost_wrapper(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            objective = "multi:softprob",
                            nrounds = 120,
                            num_class = 3,
                            eta = 0.1,
                            lambda = 0.5,
                            alpha = 0.5,
                            metricfunc = multi_class_log_loss)

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

test_that("ranger_wrapper basic use ", {

  data <- exponential_noise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- ranger_wrapper(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           train_indices = train_indices,
                           test_indices = test_indices)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("ranger_wrapper basic use with added parameters", {

  data <- exponential_noise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- ranger_wrapper(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           num.trees = 600,
                           max.depth = 4,
                           min.node.size = 3,
                           train_indices = train_indices,
                           test_indices = test_indices)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("ranger_wrapper basic use with binary Y", {

  data <- binomial_data(1000, 1, 1)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- ranger_wrapper(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           num.trees = 600,
                           train_indices = train_indices,
                           test_indices = test_indices,
                           probability = TRUE)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("ranger_wrapper basic use with categorical Y", {

  data <- simulateComplexCategorization(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- ranger_wrapper(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           num.trees = 600,
                           max.depth = 4,
                           min.node.size = 3,
                           train_indices = train_indices,
                           test_indices = test_indices,
                           probability = TRUE)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("ranger_wrapper basic use with custom metric function ", {
  rSquared <- function(data, model, test_indices) {
    actual <- data[test_indices,][['Y']]
    pred <- predict(model, data = data[test_indices,])$predictions
    sst <- sum((actual - mean(actual))^2)
    ssr <- sum((actual - pred)^2)
    metric <- 1 - (ssr / sst)
    return(metric)
  }
  data <- exponential_noise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- ranger_wrapper(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           train_indices = train_indices,
                           test_indices = test_indices,
                           metricfunc = rSquared)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------
# Testing of test.gen function. The test.gen function is the function which creates the
# null distribution

test_that("test.gen works correctly for continuous data, default method is random forest (Ranger)", {
  data <- non_lin_normal(800)
  result <- test.gen(Y = "Y", X = "X", Z = c("Z1", "Z2"), data = data)
  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})

test_that("test.gen works correctly for continuous data, default method is random forest (Ranger) with poly turned off", {
  data <- normal_data(1000)
  result <- test.gen(Y = "Y", X = "X", Z = c("Z1", "Z2"), data = data, poly = FALSE)
  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})

#-------------------------------------------------------------------------------

test_that("test.gen works correctly for continuous data, default method is random forest (Ranger)
          various parameter settings", {
            data <- normal_data(1000)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               permutation = TRUE,
                               degree = 5,
                               nrounds = 600,
                               max.depth = 6,
                               mtry = 1)
            expect_true(class(result) == "list")
            expect_true(class(mean(unlist(result))) == "numeric")

          })

#-------------------------------------------------------------------------------

test_that("test.gen works correctly for binary data, default method is random forest (Ranger)", {
  data <- binomial_data(1000, 1,1)
  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 1000,
                     data_type = "binary",
                     permutation = TRUE,
                     degree = 3,
                     nrounds = 600)

  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})
#-------------------------------------------------------------------------------

test_that("test.gen works correctly for binary data, default method is random forest (Ranger)", {
  data <- categorical_data(1000)
  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 500,
                     data_type = "categorical",
                     permutation = TRUE,
                     degree = 3,
                     nrounds = 400)

  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})

#-------------------------------------------------------------------------------

test_that("test.gen works correctly for continuous data, with Xgboost
          various parameter settings", {
            data <- normal_data(800)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               method = "xgboost",
                               permutation = TRUE,
                               degree = 3,
                               nrounds = 100,
                               max.depth = 6)
            expect_true(class(result) == "list")
            expect_true(class(mean(unlist(result))) == "numeric")

          })

#-------------------------------------------------------------------------------

test_that("test.gen works correctly for categorical data, with Xgboost
          various parameter settings", {
            data <- categorical_data(800)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               method = "xgboost",
                               data_type = "categorical",
                               permutation = TRUE,
                               degree = 3,
                               nrounds = 100,
                               num_class = 3)
            expect_true(class(result) == "list")
            expect_true(class(mean(unlist(result))) == "numeric")

          })

#-------------------------------------------------------------------------------

test_that("test.gen works correctly for continuous data, with Xgboost
          various parameter settings", {
            data <- binomial_data(800, 1, 1)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               method = "xgboost",
                               data_type = "binary",
                               permutation = TRUE,
                               degree = 3,
                               nrounds = 120)
            expect_true(class(result) == "list")
            expect_true(class(mean(unlist(result))) == "numeric")

          })

#-------------------------------------------------------------------------------

test_that("test.gen works correctly with Xgboost (takes time)", {
            data <- binomial_data(800, 1, 1)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               nperm = 1000,
                               method = "xgboost",
                               data_type = "binary",
                               permutation = TRUE,
                               degree = 5,
                               nrounds = 220)
            expect_true(class(result) == "list")
            expect_true(class(mean(unlist(result))) == "numeric")

          })

#-------------------------------------------------------------------------------


test_that("test.gen works correctly for continuous data, with GLM
          various parameter settings", {
            data <- normal_data(800)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               nperm = 1000,
                               method = "lm",
                               family = gaussian(),
                               permutation = TRUE,
                               degree = 3)
            expect_true(class(result) == "list")
            expect_true(class(mean(unlist(result))) == "numeric")

          })

#-------------------------------------------------------------------------------

test_that("test.gen works correctly for binary Y, with glm", {
  data <- binomial_data(800, 1, 1, intercept = 0.5)

  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 1000,
                     method = "lm",
                     data_type = "binary",
                     family = binomial(link = "logit"),
                     permutation = TRUE,
                     degree = 3)
  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})

#-------------------------------------------------------------------------------

test_that("test.gen works correctly for categorical Y, with glm", {
  data <- categorical_data(800)

  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 1000,
                     method = "lm",
                     data_type = "categorical",
                     permutation = TRUE,
                     degree = 3)

  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})

#-------------------------------------------------------------------------------
library(caret)
library(adabag)
caret_wrapper <- function(formula,
                             data,
                             train_indices,
                             test_indices,
                             ...) {

  model <- caret::train(formula = formula,
                            data = data[train_indices, ],
                            ...)

  actual <- data[test_indices,][['Y']]
  pred <- predict(model, newdata = data[test_indices,])
  sst <- sum((actual - mean(actual))^2)
  ssr <- sum((actual - pred)^2)
  metric <- 1 - (ssr / sst)
  return(metric)
  return(metric)
}
data <- non_lin_normal(800)

inTraining <- sample(1:nrow(dat), size = floor(0.8 * nrow(dat)), replace = FALSE)
train_indices  <- inTraining
test_indices <- setdiff(1:nrow(dat), inTraining)
train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

model <- caret::train(formula = Y ~ X + Z1 + Z2,
                          data = data[train_indices, ],
                      method = 'adaboost',
                      trcontrol = train_control)

test_that("test.gen works correctly for with custom made ML function", {

  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 200,
                     mlfunc = ADAboost_wrapper)

  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})

#-------------------------------------------------------------------------------
################## Troubleshooting perm.test() #################################


test_that("perm.test works correctly for continuous data", {
  data <- non_lin_normal(500)
  result <- perm.test(Y ~ X | Z1 + Z2, data = dat)
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
