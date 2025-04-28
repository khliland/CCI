# Test script for the CCI package
devtools::check()
devtools::build()
# install.packages("CCI_0.1.1.tar.gz", repos = NULL, type = "source")

devtools::document()

# devtools::install()
devtools::load_all()
library(CCI)
#-------------------------------------------------------------------------------
# Basic tests
#-------------------------------------------------------------------------------
test_that("CCI.test outputs a list", {
  dat <- NormalData(500)
  result <- CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, method = 'rf')
  expect_true(is.list(result))
})
test_that("CCI.test outputs a list", {
  dat <- NormalData(500)
  result <- CCI.test(formula = Y ~ X + Z1, data = dat, interaction = F, method = 'lightgbm')
  expect_true(is.list(result))
})

test_that("CCI.test outputs a list", {
  dat <- NormalData(500)
  result <- CCI.test(formula = Y ~ X + Z1, interaction = F, data = dat, method = 'xgboost')
  expect_true(is.list(result))
})

#-------------------------------------------------------------------------------
# Basic tests Binary outcome
#-------------------------------------------------------------------------------
test_that("CCI.test outputs a list", {
  dat <- NormalData(500)
  result <- CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, method = 'rf')
  expect_true(is.list(result))
})
test_that("CCI.test outputs a list", {
  dat <- NormalData(500)
  result <- CCI.test(formula = Y ~ X + Z1, data = dat, interaction = F, method = 'lightgbm')
  expect_true(is.list(result))
})

test_that("CCI.test outputs a list", {
  dat <- NormalData(500)
  result <- CCI.test(formula = Y ~ X + Z1, interaction = F, data = dat, method = 'xgboost')
  expect_true(is.list(result))
})

#-------------------------------------------------------------------------------
# Testing ML-wrapper functions
# The machine learning wrapper functions takes formula, data and indices for training and test data.
# estimates the ML model with training data, and evaluates on testing data. The different ML-wrapper takes
# different parameters depending on the ML model used. The default ML in the test is ranger (random forest).
test_that("wrapper_lightgbm outputs a metric score (basic use)", {
  dat <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100),
    x4 = rnorm(100),
    y = rnorm(100)
  )
  train_indices <- c(1:80)
  test_indices <- c(81:100)
  metric <- wrapper_lightgbm(formula = y ~ x1 + x2 + x3 + x4, data = dat, train_indices = train_indices, test_indices = test_indices, data_type = "continuous")

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
})
#-------------------------------------------------------------------------------
test_that("wrapper_lightgbm outputs a custom metric score (advance use)", {
  rSquared <- function(data, model, test_indices) {
    actual <- data[test_indices,][['Y']]
    independent <- c('X', 'Z1', 'Z2')
    testing <- data[test_indices, ]
    test_matrix <- as.matrix(testing[independent])
    pred <- predict(model, newdata = test_matrix, type = "response")
    sst <- sum((actual - mean(actual))^2)
    ssr <- sum((actual - pred)^2)
    metric <- 1 - (ssr / sst)
    return(metric)
  }

  dat <- NormalData(200)
  inTraining <- sample(1:nrow(dat), size = floor(0.8 * nrow(dat)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(dat), inTraining)
  metric <- wrapper_lightgbm(formula = Y ~ X + Z1 + Z2,
                             data = dat,
                             train_indices = train_indices,
                             test_indices = test_indices,
                             data_type = "continuous",
                             metricfunc = rSquared)

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
})
#-------------------------------------------------------------------------------
test_that("wrapper_lightgbm outputs a metric score (binary var)", {
  data <- BinaryData(400)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)
  metric <- wrapper_lightgbm(formula = Y ~ X + Z1 + Z2,
                             data = data,
                             train_indices = train_indices,
                             test_indices = test_indices,
                             data_type = "binary")
  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
})
#-------------------------------------------------------------------------------
test_that("wrapper_lightgbm outputs a metric score (binary var)", {
  data <- BivMultinominal(500)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)
  metric <- wrapper_lightgbm(formula = Y ~ X + Z1 + Z2,
                             data = data,
                             train_indices = train_indices,
                             test_indices = test_indices,
                             data_type = "categorical",
                             num_class = 4)
  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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

  metric <- wrapper_lightgbm(formula = Y ~ X + Z1 + Z2,
                             data = data,
                             train_indices = train_indices,
                             test_indices = test_indices,
                             metricfunc = multi_class_log_loss)

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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
                            nrounds = 400)

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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
                            nrounds = 500,
                            objective = "reg:squarederror")

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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
                            data_type = "binary",
                            nrounds = 120)
  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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
  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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

  data <- Multinominal(200)
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

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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
  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
})

#-------------------------------------------------------------------------------

test_that("wrapper_ranger basic use with binary Y", {

  data <- BinaryData(100)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_ranger(formula = Y ~ X + Z1 + Z2,
                           data = data,
                           num.trees = 600,
                           train_indices = train_indices,
                           test_indices = test_indices,
                           data_type = "binary")

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
})

#-------------------------------------------------------------------------------

test_that("wrapper_ranger basic use with categorical Y", {

  data <- ComplexCategorization(100)
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

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
})

#-------------------------------------------------------------------------------

test_that("wrapper_svm basic use with contionous Y", {

  data <- NormalData(100)
  inTraining <- sample(1:nrow(data), size = floor(0.8 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_svm(formula = Y ~ X + Z1 + Z2,
                        data = data,
                        data_type = "continuous",
                        train_indices = train_indices,
                        test_indices = test_indices
  )

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
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

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
})


test_that("wrapper_nnet categorical data", {

  data <- ComplexCategorization(1000)
  data$Y <- as.factor(data$Y)
  inTraining <- sample(1:nrow(data), size = floor(0.7 * nrow(data)), replace = FALSE)
  train_indices  <- inTraining
  test_indices <- setdiff(1:nrow(data), inTraining)

  metric <- wrapper_nnet(formula = Y ~ X + Z1 + Z2,
                         data = data,
                         train_indices = train_indices,
                         test_indices = test_indices,
                         data_type = "categorical",
                         size = 3)

  expect_true(is.numeric(metric))
  expect_false(is.nan(metric))
})



#-------------------------------------------------------------------------------
# Testing the tuning function
#-------------------------------------------------------------------------------

test_that("Tuning using 'rf' (default)", {
  dat <- NormalData(200)
    CCI.pretuner(formula = Y ~ X + Z1 + Z2, data = dat, method = 'rf', tune = F)
  CCI.pretuner(formula = Y ~ X + Z1 + Z2, data = dat, method = 'xgboost')
  param <- CCI.pretuner(formula = Y ~ X + Z1 + Z2, data = dat, samples = 100, method = 'lightgbm')


  # expect_true(is.numeric(parameters_rf$best_param.mtry))
  # expect_false(is.nan(parameters_rf$best_param.mtry))
})

test_that("Tuning using 'xgboost'", {
  dat <- NormalData(500)
  parameters_xgboost <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                                     data = dat,
                                     seed = 192,
                                     samples = 100,
                                     method = 'xgboost')
  args <- get_tuned_params(parameters_xgboost$best_param)

  # expect_true(is.numeric(args$best_param.nrounds))
  # expect_true(is.numeric(args$best_param.max_depth))
  # expect_true(is.numeric(args$best_param.eta))
  # expect_true(is.numeric(args$best_param.gamma))
  # expect_true(is.numeric(args$best_param.colsample_bytree))
  # expect_true(is.numeric(args$best_param.subsample))
  # expect_true(is.numeric(args$best_param.min_child_weight))

})

test_that("Tuning using 'nnet'", {
  dat <- NormalData(500)
  parameters_nnet <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                                  data = dat,
                                  samples = 200,
                                  method = 'nnet',
                                  trace = F)

  print(parameters_nnet)
  # expect_true(is.numeric(parameters_nnet$best_param.size))
  # expect_true(is.numeric(parameters_nnet$best_param.decay))
})

test_that("Tuning using 'svm'", {
  dat <- NormalData(1000)
  parameters_svm <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                                 data = dat,
                                 method = 'svm')

  print(parameters_svm)
  # expect_true(is.numeric(parameters_svm$sigma))
  # expect_true(is.numeric(parameters_svm$C))


})

test_that("Tuning using 'gpr'", {
  dat <- NormalData(1000)
  parameters_gpr <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                                 data = dat,
                                 method = 'gpr')


  expect_true(is.numeric(parameters_gpr$sigma))
})

test_that("Tuning using 'rf' and categorical data", {

  data <- TrigData(500)
  data$Y <- data$Y - 1

  parameter <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            seed = 192,
                            data_type = 'categorical',
                            method = 'rf')


  expect_true(is.numeric(parameter$mtry))
})

test_that("Tuning using 'xgboost' and categorical data", {

  data <- TrigData(700)
  data$Y <- data$Y - 1

  parameter <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            seed = 1,
                            data_type = 'categorical',
                            method = 'xgboost',
                            verboseIter = F)


  expect_true(is.numeric(parameter$mtry))
})

test_that("Tuning using 'svm' and categorical data", {

  data <- TrigData(400)
  data$Y <- data$Y - 1

  parameter <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            tune_length = 10,
                            data_type = 'categorical',
                            method = 'svm')


  expect_true(is.numeric(parameter$mtry))
})


test_that("Testing utils get_tuned_params ", {
  dat <- NormalData(1000)
  tuned_model <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                              data = dat,
                              tune_length = 5,
                              method = 'rf')

  tuned_params <- get_tuned_params(tuned_model)
  expect_true(is.numeric(tuned_params$mtry))
})

test_that("Testing utils get_tuned_params ", {
  dat <- NormalData(1000)
  tuned_model <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                              data = dat,
                              tune_length = 5,
                              method = 'xgboost')

  tuned_params <- get_tuned_params(tuned_model)
  expect_true(is.numeric(tuned_params$ets))
})

test_that("Testing utils get_tuned_params ", {
  dat <- NormalData(1000)
  tuned_model <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                              data = dat,
                              tune_length = 5,
                              method = 'svm')

  tuned_params <- get_tuned_params(tuned_model)
  expect_true(is.numeric(tuned_params$gamma))
})


#-------------------------------------------------------------------------------
test_that("clean_formula outputs correct formula", {
  clean_formula <- clean_formula(y ~ x | z + v)
  expect_true(class(clean_formula) == "formula")
  expect_equal(clean_formula, y ~ x | z + v)
})
#-------------------------------------------------------------------------------
test_that("clean_formula outputs correct formula", {
  clean_formula <- clean_formula(y ~ x + z + v)
  expect_true(class(clean_formula) == "formula")
  expect_equal(clean_formula, y ~ x | z + v)
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
# Testing of test.gen function. The test.gen function is the function which creates the
# null distribution
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for continuous data, default method is random forest (Ranger)", {

  data <- NonLinNormal(200)
  result <- test.gen(formula = Y ~ X | Z1 + Z2, data = data)
  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for continuous data, default method is random forest (Ranger) with poly turned off", {
  data <- NormalData(200)
  result <- test.gen(formula = Y ~ X | Z1 + Z2, data = data, poly = FALSE)
  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for continuous data, default method is random forest (Ranger)
          various parameter settings", {
            data <- NormalData(300)
            result <- test.gen(formula = Y ~ X | Z1 + Z2,
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
  result <- test.gen(formula = Y ~ X | Z1 + Z2,
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
  result <- test.gen(formula = Y ~ X | Z1 + Z2,
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
            result <- test.gen(formula = Y ~ X | Z1 + Z2,
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
            result <- test.gen(formula = Y ~ X | Z1 + Z2,
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
            result <- test.gen(formula = Y ~ X | Z1 + Z2,
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
            result <- test.gen(formula = Y ~ X | Z1 + Z2,
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
            result <- test.gen(formula = Y ~ X | Z1 + Z2,
                               data = data,
                               nperm = 40,
                               method = "lm",
                               data_type = "continuous",
                               family = gaussian(),
                               permutation = TRUE,
                               degree = 3)
            expect_true(class(mean(unlist(result))) == "numeric")
          })
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for binary Y, with glm", {
  data <- BinaryData(400)

  result <- test.gen(formula = Y ~ X | Z1 + Z2,
                     data = data,
                     nperm = 40,
                     method = "lm",
                     data_type = "binary",
                     family = binomial(link = "logit"),
                     permutation = FALSE,
                     degree = 3)
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
                     nperm = 25,
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
                     nperm = 50,
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
                      nperm = 25,
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
                      nrounds = 40,
                      parametric = TRUE,
                      poly = FALSE,
                      objective = "reg:pseudohubererror")
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
                      parametric = TRUE)
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------
##################### Troubleshooting CCI.test() ###############################
#-------------------------------------------------------------------------------

test_that("CII.test works correctly basic usage", {

  data <- NormalData(500)
  result <- CCI.test(formula = Y ~ X |  Z2,
                     data = data,
                     nperm = 25,
                     parametric = F)
  expect_is(result, "CCI")
})

test_that("CII.test works correctly with pre tuning", {

  data <- NormalData(700)
  result <- CCI.test(formula = Y ~ X |  Z2 + Z1,
                     data = data,
                     nperm = 100,
                     tune = T,
                     tune_length = 2,
                     parametric = T)
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------
test_that("CCI.test works binary data", {

  data <- BinaryData(500)
  result <- CCI.test(formula = Y ~ X | Z2 + Z1,
                     data = data,
                     nperm = 25,
                     data_type = 'binary',
                     parametric = F
  )
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------

test_that("CCI.test works categorical data", {
  data <- TrigData(800)
  result <- CCI.test(formula = Y ~ X | Z2 + Z1,
                     data = data,
                     nperm = 50,
                     data_type = 'categorical',
                     parametric = F
  )
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------

test_that("CII.test works correctly with pre tuning", {

  data <- ExponentialNoise(500)
  result <- CCI.test(formula = Y ~ X |  Z2,
                     data = data,
                     nperm = 100,
                     tune = T,
                     tune_length = 4,
                     method = "rf",
                     parametric = T)
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------

test_that("CII.test works correctly with pre tuning", {

  data <- BinaryData(700)
  data$Y <- as.factor(data$Y)
  result <- CCI.test(formula = Y ~ X |  Z2,
                     data = data,
                     nperm = 100,
                     tune = T,
                     tune_length = 10,
                     method = "xgboost",
                     data_type = 'binary',
                     parametric = T)
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------

test_that("CII.test works correctly basic usage", {
  set.seed(11)
  data <- NormalData(500)
  result <- CCI.test(formula = Y ~ X |  Z2,
                     data = data,
                     nperm = 25,
                     parametric = T)
  expect_is(result, "CCI")
})


#-------------------------------------------------------------------------------

test_that("CCI.test works categorical data with xgboost", {
  data <- TrigData(500)
  data$Y <- data$Y - 1
  result <- CCI.test(formula = Y ~ X | Z2,
                     data = data,
                     p = 0.7,
                     method = "xgboost",
                     nperm = 10,
                     data_type = 'categorical',
                     num_class = 3,
                     parametric = F
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works basic with lightgbm", {
  data <- NormalData(5000)
  result <- CCI.test(formula = Y ~ X | Z2 + Z1,
                     data = data,
                     p = 0.7,
                     method = "lightgbm",
                     nperm = 100,
                     parametric = T
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
# HER
test_that("CCI.test works categorical data", {
  data <- TrigData(500)
  head(data)
  data$Y <- data$Y - 1
  data$X <- as.factor(data$X)

  result <- CCI.test(formula = Y ~ X | Z2 + Z1,
                     data = data,
                     nperm = 100,
                     data_type = 'categorical',
                     parametric = T,
                     tune = T,
                     verboseIter = T
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
# HER
test_that("CCI.test works nnet", {
  data <- NormalData(500)

  result <- CCI.test(formula = Y ~ X | Z2 + Z1,
                     data = data,
                     nperm = 100,
                     parametric = T,
                     tune = T,
                     method = 'nnet'
  )
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------
test_that("CCI.test works rejecting a wrong null with lm", {

  data <- NormalData(500)
  result <- CCI.test(formula = Y ~ X | Z2,
                     p = 0.6,
                     data = data,
                     nperm = 25,
                     method = 'lm',
                     family = gaussian(),
                     data_type = "continuous",
                     parametric = TRUE
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works categorical data with glm", {

  data <- TrigData(500)
  data$Y <- data$Y - 1
  result <- CCI.test(formula = Y ~ X | Z2,
                     data = data,
                     p = 0.7,
                     method = "lm",
                     nperm = 25,
                     data_type = 'categorical',
                     parametric = T
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works rejecting a wrong null with xgboost", {
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
                     nperm = 50,
                     parametric = T
  )

  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------

test_that("CCI.test works with custom wrapper function, calculating R-squared", {
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
