devtools::check()
devtools::build
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
  dist <- rnorm(100)
  test_statistic <- rnorm(1)
  p_value <- get_pvalues(dist = dist, test_statistic = test_statistic, tail = "right")
  p_value
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
test_that("wrapper_glm outputs a metric score (basic use)", {
  dat <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100),
    x4 = rnorm(100),
    y = rnorm(100)
  )
  train_indices <- c(1:80)
  test_indices <- c(81:100)
  metric <- wrapper_glm(formula = y ~ x1 + x2 + x3 + x4, data = dat, train_indices = train_indices, test_indices = test_indices, data_type = "continuous", family = gaussian(link = "identity"))
  expect_true(class(metric) == "numeric")
})
#-------------------------------------------------------------------------------
test_that("wrapper_glm outputs a custom metric score (advance use)", {
  rSquared <- function(data, model, test_indices) {
    actual <- data[test_indices,][['Y']]
    pred <- predict.glm(model, newdata = data[test_indices,])
    sst <- sum((actual - mean(actual))^2)
    ssr <- sum((actual - pred)^2)
    metric <- 1 - (ssr / sst)
    return(metric)
  }

  dat <- NormalData(200)
  inTraining <- sample(1:nrow(dat), size = floor(0.8 * nrow(dat)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(dat), inTraining)
  metric <- wrapper_glm(formula = Y ~ X + Z1 + Z2,
                        data = dat,
                        train_indices = train_indices,
                        test_indices = test_indices,
                        data_type = "continuous",
                        family = gaussian(link = "identity"),
                        metricfunc = rSquared)
  expect_true(class(metric) == "numeric")
})
#-------------------------------------------------------------------------------
test_that("wrapper_glm outputs a metric score (binary var)", {
  data <- BinaryData(300)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)
  metric <- wrapper_glm(formula = Y ~ X + Z1 + Z2,
                        data = data,
                        train_indices = train_indices,
                        test_indices = test_indices,
                        data_type = "binary",
                        family = binomial(link = "logit"))
  expect_true(class(metric) == "numeric")
})
#-------------------------------------------------------------------------------
# Example of a custom function for calculating log loss with categorical outcome
#-------------------------------------------------------------------------------
test_that("wrapper_multinom", {
  data <- InteractiondData(1000)

  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_multinom(formula = Y ~ X + Z1 + Z2,
                             data = data,
                             train_indices = train_indices,
                             test_indices = test_indices)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------
# Example of a custom function for calculating log loss with categorical outcome
#-------------------------------------------------------------------------------

test_that("wrapper_multinom outputs a custom log loss metric score", {
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

  data <- Multinominal(1000)
  data$X <- as.factor(data$X)
  data$Y <- as.factor(data$Y)

  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_multinom(formula = Y ~ X + Z1 + Z2,
                        data = data,
                        train_indices = train_indices,
                        test_indices = test_indices,
                        metricfunc = multi_class_log_loss)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------
test_that("wrapper_xgboost rmse output", {

  data <- UniformNoise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_xgboost(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            data_type = "continuous",
                            nrounds = 120)

  expect_true(class(metric) == "numeric")
})
#-------------------------------------------------------------------------------
test_that("wrapper_xgboost rmse output", {

  data <- UniformNoise(500)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)
  metric <- wrapper_xgboost(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            nrounds = 100,
                            objective = "reg:squarederror")

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------
test_that("wrapper_xgboost rmse output using a different objective", {

  data <- UniformNoise(1000)
  data$Y <- abs(data$Y)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_xgboost(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            objective = "reg:squaredlogerror",
                            nrounds = 120)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------
test_that("wrapper_xgboost rmse output with reg:pseudohubererror and various parameter settings", {

  data <- UniformNoise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_xgboost(formula = Y ~ X + Z1 + Z2,
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

test_that("wrapper_xgboost rmse output with reg:squarederror, various parameter settings and custom performance metric (R-squared)", {
  rSquared <- function(data, model, test_indices, test_matrix) {
    actual <- data[test_indices,][['Y']]
    pred <- predict(model, newdata = test_matrix)
    sst <- sum((actual - mean(actual))^2)
    ssr <- sum((actual - pred)^2)
    metric <- 1 - (ssr / sst)
    return(metric)
  }

  data <- UniformNoise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_xgboost(formula = Y ~ X + Z1 + Z2,
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
test_that("wrapper_xgboost Kappa score output", {

  data <- BinaryData(200)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_xgboost(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            objective = "binary:logistic",
                            nrounds = 120)
  expect_true(class(metric) == "numeric")
})
#-------------------------------------------------------------------------------
test_that("wrapper_xgboost Kappa score output", {

  data <- BinaryData(500)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_xgboost(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            train_indices = train_indices,
                            test_indices = test_indices,
                            data_type = "binary",
                            nrounds = 120)
  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("wrapper_xgboost log loss score output", {
  multi_class_log_loss <- function(data, model, test_indices, test_matrix) {
    eps = 0.001
    pred <- predict(model, newdata = test_matrix)
    actual <- data[test_indices,][['Y']] #Hard coded, must change if you have a different formula
    actual_matrix <- model.matrix(~ factor(actual) - 1)
    num_classes <- length(unique(actual))
    pred_matrix <- matrix(pred, ncol = num_classes, byrow = TRUE)

    pred_matrix <- pmax(pmin(pred_matrix, 1 - eps), eps)
    log_loss <- -sum(actual_matrix * log(pred_matrix)) / nrow(pred_matrix)
    return(log_loss)
  }

  data <- Multinominal(1000)
  data$Y <- as.numeric(as.factor(data$Y))-1
  data$X <- as.numeric(as.factor(data$X))-1

  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_xgboost(formula = Y ~ X + Z1 + Z2,
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

test_that("wrapper_xgboost Kappa score output", {

  data <- ComplexCategorization(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_xgboost(formula = Y ~ X + Z1 + Z2,
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

test_that("wrapper_ranger basic use ", {

  data <- ExponentialNoise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_ranger(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           train_indices = train_indices,
                           test_indices = test_indices,
                           data_type = "continuous")

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("wrapper_ranger basic use with added parameters", {

  data <- ExponentialNoise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_ranger(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           num.trees = 600,
                           max.depth = 4,
                           min.node.size = 3,
                           data_type = "continuous",
                           train_indices = train_indices,
                           test_indices = test_indices)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("wrapper_ranger basic use with binary Y", {

  data <- BinaryData(500)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_ranger(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           num.trees = 600,
                           train_indices = train_indices,
                           test_indices = test_indices,
                           data_type = "binary")

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("wrapper_ranger basic use with categorical Y", {

  data <- ComplexCategorization(500)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_ranger(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           num.trees = 600,
                           max.depth = 4,
                           min.node.size = 3,
                           train_indices = train_indices,
                           test_indices = test_indices,
                           data_type = "categorical"
  )

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("wrapper_svm basic use with contionous Y", {

  data <- NormalData(500)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_svm(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           data_type = "continuous",
                           train_indices = train_indices,
                           test_indices = test_indices
                        )

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("wrapper_svm basic use with binary Y", {

  data <- BinaryData(500)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_svm(formula = Y ~ X + Z1 + Z2,
                        data = data,
                        data_type = "binary",
                        train_indices = train_indices,
                        test_indices = test_indices
  )

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("wrapper_svm basic use with categorical Y", {

  data <- ComplexCategorization(500)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_svm(formula = Y ~ X + Z1 + Z2,
                        data = data,
                        data_type = "categorical",
                        train_indices = train_indices,
                        test_indices = test_indices
  )

  expect_true(class(metric) == "numeric")
})


#-------------------------------------------------------------------------------

test_that("wrapper_ranger basic use with custom metric function ", {
  rSquared <- function(data, model, test_indices) {
    actual <- data[test_indices,][['Y']]
    pred <- predict(model, data = data[test_indices,])$predictions
    sst <- sum((actual - mean(actual))^2)
    ssr <- sum((actual - pred)^2)
    metric <- 1 - (ssr / sst)
    return(metric)
  }
  data <- ExponentialNoise(1000)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_ranger(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           train_indices = train_indices,
                           test_indices = test_indices,
                           data_type = "continuous",
                           metricfunc = rSquared)

  expect_true(class(metric) == "numeric")
})

#-------------------------------------------------------------------------------

test_that("wrapper_nnet basic use", {

  data <- NormalData(500)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_nnet(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           train_indices = train_indices,
                           test_indices = test_indices,
                           data_type = "continuous",
                           size = 3)

  expect_true(class(metric) == "numeric")
})


#-------------------------------------------------------------------------------
# Testing of test.gen function. The test.gen function is the function which creates the
# null distribution
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for continuous data, default method is random forest (Ranger)", {
  set.seed(1)
  data <- NonLinNormal(200)
  result <- test.gen(Y = "Y", X = "X", Z = c("Z1", "Z2"), data = data)
  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for continuous data, default method is random forest (Ranger) with poly turned off", {
  data <- NormalData(200)
  result <- test.gen(Y = "Y", X = "X", Z = c("Z1", "Z2"), data = data, poly = FALSE)
  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for continuous data, default method is random forest (Ranger)
          various parameter settings", {
            data <- NormalData(300)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               permutation = TRUE,
                               nperm = 50,
                               degree = 5,
                               nrounds = 600,
                               max.depth = 6,
                               mtry = 1)
            expect_true(class(result) == "list")
            expect_true(class(mean(unlist(result))) == "numeric")

          })
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for binary data, default method is random forest (Ranger)", {
  data <- BinaryData(200)
  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 40,
                     data_type = "binary",
                     permutation = TRUE,
                     degree = 3,
                     nrounds = 600)

  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for categorical data, default method is random forest (Ranger)", {
  data <- InteractiondData(200)
  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 50,
                     data_type = "categorical",
                     permutation = TRUE,
                     degree = 3,
                     nrounds = 400)

  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for continuous data, with Xgboost various parameter settings", {
            data <- NormalData(800)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               method = "xgboost",
                               permutation = TRUE,
                               degree = 3,
                               nperm = 40,
                               nrounds = 100,
                               max.depth = 6)
            expect_true(class(result) == "list")
            expect_true(class(mean(unlist(result))) == "numeric")

          })
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for categorical data, with Xgboost
          setting the num_class parameter", {
            data <- InteractiondData(400)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               method = "xgboost",
                               data_type = "categorical",
                               permutation = TRUE,
                               degree = 3,
                               nperm = 40,
                               nrounds = 100,
                               num_class = 4)
            expect_true(class(result) == "list")
            expect_true(class(mean(unlist(result))) == "numeric")

          })
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for binary outcome data using Xgboost", {
            data <- BinaryData(800)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               method = "xgboost",
                               data_type = "binary",
                               nperm = 40,
                               permutation = TRUE,
                               degree = 3,
                               nrounds = 120)
            expect_true(class(result) == "list")
            expect_true(class(mean(unlist(result))) == "numeric")

          })
#-------------------------------------------------------------------------------
test_that("test.gen works correctly with Xgboost", {
            data <- BinaryData(500)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               nperm = 40,
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
            data <- NormalData(400)
            result <- test.gen(Y = "Y",
                               X = "X",
                               Z = c("Z1", "Z2"),
                               data = data,
                               nperm = 40,
                               method = "lm",
                               family = gaussian(),
                               permutation = TRUE,
                               degree = 3)
            expect_true(class(result) == "list")
            expect_true(class(mean(unlist(result))) == "numeric")
          })
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for binary Y, with glm", {
  data <- BinaryData(400)

  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 40,
                     method = "lm",
                     data_type = "binary",
                     family = binomial(link = "logit"),
                     permutation = FALSE,
                     degree = 3)
  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for categorical Y, with glm", {
  data <- InteractiondData(400)

  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 50,
                     method = "lm",
                     data_type = "categorical",
                     permutation = TRUE,
                     degree = 3)

  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")
})
#-------------------------------------------------------------------------------
test_that("test.gen works correctly with  SVM", {
  data <- NormalData(600)

  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 100,
                     method = "svm")

  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")
})
#-------------------------------------------------------------------------------
test_that("test.gen works correctly withSVM categorical", {
  data <- InteractiondData(600)

  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 100,
                     data_type = "categorical",
                     method = "svm")

  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")
})

#-------------------------------------------------------------------------------
# Creating a wrapper function using the caret package with cross-validation
bagging_wrapper <- function(formula,
                          data,
                          train_indices,
                          test_indices,
                          ...) {
  training <- data[train_indices, ]
  testing <- data[test_indices, ]

  model <- ipred::bagging(form = formula,
                        data = training,
                            ...)

  actual <- testing[['Y']]
  pred <- predict(model, newdata = testing)
  sst <- sum((actual - mean(actual))^2)
  ssr <- sum((actual - pred)^2)
  metric <- 1 - (ssr / sst)
  return(metric)
}

#-------------------------------------------------------------------------------
test_that("test.gen works correctly for with custom made ML function called bagging_wrapper", {
  data <- NormalData(500)
  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 50,
                     nbag = 50,
                     mlfunc = bagging_wrapper)

  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})
#-------------------------------------------------------------------------------
rSquared <- function(data, model, test_indices) {
  actual <- data[test_indices,][['Y']]
  pred <- predict(model, data = data[test_indices,])$predictions
  sst <- sum((actual - mean(actual))^2)
  ssr <- sum((actual - pred)^2)
  metric <- 1 - (ssr / sst)
  return(metric)
}
test_that("test.gen works correctly using metricfunc", {
  data <- NonLinNormal(500)

  result <- test.gen(Y = "Y",
                     X = "X",
                     Z = c("Z1", "Z2"),
                     data = data,
                     nperm = 40,
                     metricfunc = rSquared)

  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})

#-------------------------------------------------------------------------------
################## Troubleshooting perm.test() #################################
#-------------------------------------------------------------------------------

test_that("perm.test works correctly in the simplest case", {
  data <- NonLinNormal(500)
  result <- perm.test(Y ~ X | Z1 + Z2, data = data, nperm = 40)
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("perm.test works with metricfunc", {
  data <- NonLinNormal(200)
  result <- perm.test(Y ~ X | Z1 + Z2,
                      data = data,
                      nperm = 40,
                      metricfunc = rSquared,
                      tail = 'right')
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("perm.test works with mlfunc", {
  data <- PoissonNoise(300)
  result <- perm.test(Y ~ X + Z1 + Z2,
                      data = data,
                      coob = T,
                      nperm = 50,
                      mlfunc = bagging_wrapper,
                      tail = "right")
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("perm.test works correctly", {
  data <- NonLinNormal(500)
  result <- perm.test(Y ~ X | Z1 + Z2,
                      data = data,
                      nperm = 25,
                      method = "xgboost",
                      nrounds = 90)
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("perm.test works correctly", {
  data <- NonLinNormal(500)
  result <- perm.test(formula = Y ~ X + Z1 + Z2,
                      data = data,
                      p = 0.7,
                      nperm = 25,
                      data_type = "continuous",
                      method = "xgboost",
                      nrounds = 80,
                      parametric = TRUE,
                      poly = FALSE,
                      objective = "reg:pseudohubererror",
                      seed =3030)
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("perm.test works correctly with dagitty object", {
  data <- NonLinNormal(500)
  dag <- dagitty::dagitty('dag {
  X
  Y
  Z1
  Z2
  Z1 -> X
  Z1 -> Y
  Z2 -> X
  Z2 -> Y
}')

  result <- perm.test(formula = NULL,
                      dag = dag,
                      dag_n = 1,
                      data = data,
                      p = 0.7,
                      nperm = 40,
                      data_type = "continuous",
                      parametric = TRUE,
                      seed = 33)
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------
##################### Troubleshooting CCI.test() ###############################
#-------------------------------------------------------------------------------

test_that("CII.test works correctly basic usage", {
  set.seed(8)
  data <- NormalData(500)
  result <- CCI.test(formula = Y ~ X |  Z2,
                     data = data,
                     nperm = 250,
                     parametric = F)
  expect_is(result, "CCI")
})

test_that("CII.test works correctly basic usage", {
  set.seed(11)
  data <- NormalData(500)
  result <- CCI.test(formula = Y ~ X |  Z2,
                     data = data,
                     nperm = 250,
                     parametric = T)
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------
test_that("CCI.test works binary data", {
  set.seed(1)
  data <- BinaryData(500)
  result <- CCI.test(formula = Y ~ X | Z2,
                     data = data,
                     nperm = 250,
                     data_type = 'binary',
                     parametric = F
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works binary data", {
  set.seed(1)
  data <- BinaryData(500)
  result <- CCI.test(formula = Y ~ X | Z2,
                     data = data,
                     nperm = 250,
                     data_type = 'binary',
                     parametric = F
  )
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------
test_that("CCI.test works categorical data", {
  set.seed(1)
  data <- BinaryData(200)
  result <- CCI.test(formula = Y ~ X | Z2,
                     data = data,
                     nperm = 250,
                     data_type = 'binary',
                     parametric = T
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------

test_that("CCI.test works categorical data", {
  set.seed(91)
  data <- TrigData(800)
  result <- CCI.test(formula = Y ~ X | Z2,
                     data = data,
                     nperm = 250,
                     data_type = 'categorical',
                     parametric = F
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------

test_that("CCI.test works categorical data", {
  set.seed(91)
  data <- TrigData(800)
  result <- CCI.test(formula = Y ~ X | Z2,
                     data = data,
                     nperm = 250,
                     data_type = 'categorical',
                     parametric = T
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------

test_that("CCI.test works categorical data with xgboost", {
  set.seed(90)
  data <- TrigData(500)
  data$Y <- data$Y - 1
  result <- CCI.test(formula = Y ~ X | Z2,
                     data = data,
                     p = 0.7,
                     method = "xgboost",
                     nperm = 250,
                     data_type = 'categorical',
                     num_class = 3,
                     parametric = F
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works rejecting a wrong null with lm", {
  set.seed(9)
  data <- NormalData(500)
  result <- CCI.test(formula = Y ~ X | Z2,
                     p = 0.6,
                     data = data,
                     nperm = 500,
                     method = 'lm',
                     family = gaussian(),
                     data_type = "continuous",
                     parametric = TRUE
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works categorical data with glm", {
  set.seed(90)
  data <- TrigData(500)
  data$Y <- data$Y - 1
  result <- CCI.test(formula = Y ~ X | Z2,
                     data = data,
                     p = 0.7,
                     method = "lm",
                     nperm = 250,
                     data_type = 'categorical',
                     parametric = T
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works rejecting a wrong null with xgboost", {
  set.seed(9)
  data <- NonLinNormal(500)
  result <- CCI.test(formula = Y ~ X | Z2,
                     p = 0.7,
                     data = data,
                     nperm = 50,
                     method = 'xgboost',
                     data_type = "continuous",
                     parametric = TRUE
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works categorical data, wrong null", {
  set.seed(911)
  data <- ExpLogData(500)
  result <- CCI.test(formula = Y ~ X | Z2 + Z1,
                     p = 0.7,
                     data = data,
                     nperm = 40,
                     data_type = 'categorical',
                     parametric = FALSE
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works categorical data, wrong null", {
  set.seed(911)
  data <- ExpLogData(1000)
  result <- CCI.test(formula = Y ~ X | Z2,
                     p = 0.7,
                     data = data,
                     nperm = 40,
                     method = "lm",
                     data_type = 'categorical',
                     parametric = F
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works with custom performance metric", {
  set.seed(1911)
  data <- SinusoidalData(500)
  rSquared <- function(data, model, test_indices) {
    actual <- data[test_indices,][['Y']]
    pred <- predict(model, data = data[test_indices,])$predictions
    sst <- sum((actual - mean(actual))^2)
    ssr <- sum((actual - pred)^2)
    metric <- 1 - (ssr / sst)
    return(metric)
  }
  result <- CCI.test(formula = Y ~ X | Z2 + Z1,
                     p = 0.7,
                     data = data,
                     nperm = 40,
                     parametric = T,
                     metricfunc = rSquared,
                     tail = 'right'
  )

  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works with custom performance metric, wrong null", {
  set.seed(1939)
  data <- SinusoidalData(200)
  rSquared <- function(data, model, test_indices) {
    actual <- data[test_indices,][['Y']]
    pred <- predict(model, data = data[test_indices,])$predictions
    sst <- sum((actual - mean(actual))^2)
    ssr <- sum((actual - pred)^2)
    metric <- 1 - (ssr / sst)
    return(metric)
  }
  result <- CCI.test(formula = Y ~ X | Z2,
                     p = 0.7,
                     data = data,
                     nperm = 40,
                     parametric = T,
                     metricfunc = rSquared,
                     tail = 'right'
  )

  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works with dagitty", {
  set.seed(1990)
  data <- SinusoidalData(200)
  dag <- dagitty::dagitty('dag {
  X
  Y
  Z1
  Z2
  Z1 -> X
  Z1 -> Y
  Z2 -> X
  Z2 -> Y
}')
plot(dag)

result <- CCI.test(formula = Y ~ X | Z2 + Z1,
                     p = 0.7,
                     data = data,
                     dag = dag,
                     dag_n = 1,
                     nperm = 500,
                     parametric = T
  )

  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------

test_that("CCI.test works with custom wrapper function, calculating R-squared", {
  set.seed(3)
  data <- SinusoidalData(400)
  bagging_wrapper <- function(formula,
                              data,
                              train_indices,
                              test_indices,
                              ...) {
    training <- data[train_indices, ]
    testing <- data[test_indices, ]

    model <- ipred::bagging(form = formula,
                            data = training,
                            ...)

    actual <- testing[['Y']]
    pred <- predict(model, newdata = testing)
    sst <- sum((actual - mean(actual))^2)
    ssr <- sum((actual - pred)^2)
    metric <- 1 - (ssr / sst)
    return(metric)
  }


  result <- CCI.test(formula = Y ~ X | Z1 + Z2,
                     p = 0.7,
                     data = data,
                     nperm = 60,
                     parametric = F,
                     seed = 9,
                     mlfunc = bagging_wrapper,
                     tail = "right"
  )

  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------
##################### QQplot() ###############################
#-------------------------------------------------------------------------------

data <- SinusoidalData(1000)
result <- CCI.test(formula = Y ~ X | Z1 + Z2,
                   p = 0.7,
                   data = data,
                   nperm = 500,
                   parametric = T
)
QQplot(result)


result <- CCI.test(formula = Y ~ X | Z2,
                   p = 0.7,
                   data = data,
                   nperm = 100,
                   parametric = T
)
QQplot(result)
