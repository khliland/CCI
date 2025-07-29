# Test script for the CCI package
devtools::check()
devtools::check_win_devel()
devtools::document()
devtools::clean_dll()
devtools::build(path = "C:/CCI")
# install.packages("CCI_0.1.1.tar.gz", repos = NULL, type = "source")
devtools::build_vignettes()

devtools::install()
devtools::load_all()
library(CCI)
#-------------------------------------------------------------------------------
# Basic tests CCI.test()
#-------------------------------------------------------------------------------
test_that("CCI.test outputs a list", {
  set.seed(8)
  dat <- NormalData(500)
  result <- CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, method = 'rf', interaction = F)
  summary(result)
  expect_true(is.list(result))
})

summary(result)
plot(result)

test_that("CCI.test outputs a list", {
  set.seed(1)
  dat <- NormalData(500)
  result <- CCI.test(formula = Y ~ X + Z1 + Z2, interaction = F, data = dat, method = 'xgboost')
  summary(result)
  expect_true(is.list(result))
})

test_that("CCI.test outputs a list", {
  set.seed(1)
  dat <- NormalData(250)
  result <- CCI.test(formula = Y ~ X + Z1 + Z2, interaction = F, data = dat, method = 'svm')
  summary(result)
  expect_true(is.list(result))
})

#-------------------------------------------------------------------------------
# Basic tests CCI.pretuner
#-------------------------------------------------------------------------------
test_that("CCI.pretuner outputs a list", {
  dat <- NormalData(500)
  CCI.pretuner(formula = Y ~ X + Z1 + Z2, data = dat, method = 'xgboost', samples = 5)
  CCI.pretuner(formula = Y ~ X + Z1 + Z2, data = dat, method = 'rf')

  expect_true(is.list(result))
})

#-------------------------------------------------------------------------------
# Basic tests QQplot
#-------------------------------------------------------------------------------
test_that("QQplot should produce a QQplot", {
  dat <- NormalData(500)
  cci_obj <- CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, nperm = 100)
  QQplot(cci_obj)
  expect_true(is.list(result))
})

#--------------------
# With tuning
#--------------------

test_that("CCI.test outputs a list", {
  dat <- NonLinNormal(500)
  result <- CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, interaction = F, method = 'rf', tune = TRUE)
  expect_true(is.list(result))
})

test_that("CCI.test outputs a list", {
  dat <- NonLinNormal(200)
  result <- CCI.test(formula = Y ~ X + Z1, interaction = F, nperm = 60, data = dat, method = 'xgboost', tune = TRUE)
  expect_true(is.list(result))
})

test_that("CCI.test outputs a list", {
  dat <- NonLinNormal(200)
  result <- CCI.test(formula = Y ~ X + Z1, interaction = F, nperm = 60, data = dat, method = 'svm', tune = TRUE)
  expect_true(is.list(result))
})


#-------------------------------------------------------------------------------
# Basic tests Binary outcome
#-------------------------------------------------------------------------------
test_that("CCI.test outputs a list", {
  set.seed(1985)
  dat <- BinaryData(500)
  result <- CCI.test(formula = Y ~ X + Z1, data = dat, method = 'xgboost', parametric = T)
  summary(result)
  plot(result)
  expect_true(is.list(result))
})
test_that("CCI.test outputs a list", {
  set.seed(1985)
  dat <- BinaryData(500)
  result <- CCI.test(formula = Y ~ X + Z1, data = dat, interaction = F, method = 'rf')
  summary(result)
  plot(result)
  expect_true(is.list(result))
})

test_that("CCI.test outputs a list", {
  set.seed(1985)
  dat <- BinaryData(500)
  result <- CCI.test(formula = Y ~ X + Z1, interaction = F, data = dat, method = 'svm', metric = 'Kappa')
  summary(result)
  plot(result)
  expect_true(is.list(result))
})


#-------------------------------------------------------------------------------
# Basic tests Categorical outcome
#-------------------------------------------------------------------------------
test_that("CCI.test outputs a list", {
  dat <- NonLinearCategorization(600, d = 2)
  dat$Y <- as.factor(dat$Y)
  result <- CCI.test(formula = Y ~ X + Z, data = dat, method = 'rf', interaction = F)
  summary(result)

  expect_true(is.list(result))
})
test_that("CCI.test outputs a list", {
  dat <- NonLinearCategorization(800, d = 2)
  dat$Y <- as.factor(dat$Y)
  result <- CCI.test(formula = Y ~ X + Z,
                     data = dat,
                     interaction = F,
                     method = 'xgboost')
  summary(result)
                     expect_true(is.list(result))
})

test_that("CCI.test outputs a list", {
  dat <- NonLinearCategorization(800, d = 2)
  dat$Y <- as.factor(dat$Y)
  result <- CCI.test(formula = Y ~ X + Z,
                     interaction = F,
                     data = dat,
                     method = 'svm')
  expect_true(is.list(result))
})

#-----------------------------------------------------
# Direction function
#-----------------------------------------------------

test_that('CCI.direction', {
  dat <- SineGaussianBiv(N = 500, a = 2, d = 1)
  result <- CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, choose_direction = T)
  expect_true(inherits(result, "formula"))
})

test_that('CCI.direction', {
  dat <- SineGaussianBiv(N = 500, a = 2, d = 1)
  CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, method = 'xgboost', choose_direction = TRUE)
  expect_true(inherits(result, "formula"))
})

test_that('CCI.direction', {
  dat <- SineGaussianBiv(N = 500, a = 2, d = 1)
  CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, method = 'svm', choose_direction = TRUE)
  expect_true(inherits(result, "formula"))
})

#-----------------------------------------------------
# DAG example
#-----------------------------------------------------

test_that('Dagitty', {

  dag <- dagitty::dagitty("dag {
    Z1 -> Y
    Z2 -> Y
    Z1 -> X
    Z2 -> X
  }")
  dagitty::impliedConditionalIndependencies(dag)
  dat <- NormalData(N = 500)
  result <- CCI.test(dag = dag, data = dat)

})
test_that('Dagitty with direction', {
  dag <- dagitty::dagitty("dag {
    Z1 -> Y
    Z2 -> Y
    Z1 -> X
    Z2 -> X
  }")
  dagitty::impliedConditionalIndependencies(dag)
  dat <- NormalData(N = 500)
  result <- CCI.test(dag = dag, data = dat, choose_direction = TRUE)

})

#-------------------------------------------------------------------------------
# Testing the tuning function
#-------------------------------------------------------------------------------

test_that("Tuning using 'rf' (default)", {
  dat <- NormalData(200)
  CCI.pretuner(formula = Y ~ X + Z1 + Z2, data = dat, method = 'rf', tune = F, tune_length = 3)
  CCI.pretuner(formula = Y ~ X + Z1 + Z2, data = dat, method = 'xgboost')
  CCI.pretuner(formula = Y ~ X + Z1 + Z2, data = dat, samples = 100, method = 'svm')
})


test_that("Tuning using 'rf' and categorical data", {

  data <- TrigData(500)
  data$Y <- as.numeric(as.factor(data$Y))
  data$X <- as.numeric(as.factor(data$X))
  parameter <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            seed = 192,
                            method = 'rf')


  expect_true(is.numeric(parameter$mtry))
})

test_that("Tuning using 'xgboost' and categorical data", {

  data <- TrigData(700)
  data$Y <- as.numeric(as.factor(data$Y)) - 1
  data$X <- as.numeric(as.factor(data$X)) - 1
  parameter <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            metric = 'Kappa',
                            method = 'xgboost',
                            verboseIter = F)

  parameter$warnings
  expect_true(is.numeric(parameter$mtry))
})

test_that("Tuning using 'svm' and categorical data", {

  data <- TrigData(400)
  data$Y <- as.numeric(as.factor(data$Y)) - 1
  data$X <- as.numeric(as.factor(data$X)) - 1
  parameter <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            tune_length = 10,
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
                              samples = 5,
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
  result <- test.gen(formula = Y ~ X | Z1 + Z2, data = data, metric = "RMSE")
  expect_true(class(result) == "list")
  expect_true(class(mean(unlist(result))) == "numeric")

})
#-------------------------------------------------------------------------------
test_that("test.gen works correctly for continuous data, default method is random forest (Ranger) with poly turned off", {
  data <- NormalData(200)
  result <- test.gen(formula = Y ~ X | Z1 + Z2, data = data, poly = FALSE, metric = 'RMSE')
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
                               mtry = 1,
                               metric = "RMSE")
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
                     parametric = F
  )
  summary(result)
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------

test_that("CCI.test works categorical data", {
  data <- TrigData(800)
  data$Y <- as.numeric(as.factor(data$Y)) - 1
  data$X <- as.numeric(as.factor(data$X)) - 1
  result <- CCI.test(formula = Y ~ X | Z2 + Z1,
                     data = data,
                     nperm = 50,
                     metric = "Kappa",
                     parametric = F
  )
  summary(result)
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------

test_that("CII.test works correctly with pre tuning", {

  data <- ExponentialNoise(500)
  result <- CCI.test(formula = Y ~ X |  Z2,
                     data = data,
                     choose_direction = TRUE,
                     method = "rf",
                     parametric = T)
  warningexpect_is(result, "CCI")
})

#-------------------------------------------------------------------------------

test_that("CII.test works correctly with pre tuning", {

  data <- BinaryData(700)
  data$Y <- as.numeric(as.factor(data$Y))-1
  result <- CCI.test(formula = Y ~ X |  Z2,
                     data = data,
                     nperm = 10,
                     tune = T,
                     tune_length = 10,
                     method = "xgboost",
                     metric = "RMSE",
                     parametric = T)
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------

test_that("CII.test works correctly basic usage", {
  set.seed(11)
  data <- NormalData(500)
  result <- CCI.test(formula = Y ~ X |  Z2,
                     data = data)
  expect_is(result, "CCI")
})


#-------------------------------------------------------------------------------

test_that("CCI.test works categorical data with xgboost", {
  data <- TrigData(500)
  data$Y <- as.factor(data$Y)
  data$X <- as.factor(data$X)
  result <- CCI.test(formula = Y ~ X | Z2,
                     data = data,
                     p = 0.7,
                     method = "xgboost",
                     nperm = 10,
                     metric = 'Kappa',
                     num_class = 3,
                     parametric = F
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works basic", {
  data <- NormalData(5000)
  result <- CCI.test(formula = Y ~ X | Z2 + Z1,
                     data = data,
                     p = 0.7,
                     subsample = 0.2,
                     method = "svm",
                     nperm = 100,
                     parametric = T
  )
  summary(result)
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
# HER
test_that("CCI.test works categorical data", {
  data <- TrigData(500)

  data$Y <- as.factor(data$Y)
  data$X <- as.factor(data$X)

  result <- CCI.test(formula = Y ~ X | Z2 + Z1,
                     data = data,
                     nperm = 100,
                     metric = 'Kappa',
                     parametric = T,
                     tune = T,
                     verboseIter = T
  )
  expect_is(result, "CCI")
})

#-------------------------------------------------------------------------------
test_that("CCI.test works rejecting a wrong null with xgboost", {
  data <- NonLinNormal(500)
  result <- CCI.test(formula = Y ~ X | Z2,
                     p = 0.7,
                     data = data,
                     method = 'xgboost',
                     parametric = TRUE
  )
  expect_is(result, "CCI")
})
#-------------------------------------------------------------------------------
test_that("CCI.test works categorical data, wrong null", {
  data <- ExpLogData(500)
  data$Y <- as.numeric(as.factor(data$Y)) - 1
  data$X <- as.numeric(as.factor(data$X)) - 1
  result <- CCI.test(formula = Y ~ X | Z1,
                     p = 0.7,
                     data = data,
                     nperm = 40,
                     method = "xgboost",
                     parametric = FALSE
  )
  summary(result)
  plot(result)
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





devtools::load_all()
library(CCI)
library(caret)
caret_wrapper <- function(formula,
                          data,
                          train_indices,
                          test_indices,
                          caret_method,
                          caret_metric,
                          ...) {

  training_data <- data[train_indices, ]
  test_data <- data[test_indices, ]
  ctrl <- caret::trainControl(method = "none")
  model <- caret::train(formula,
                        data = training_data,
                        method = caret_method,
                        trControl = ctrl,
                        verbose = F,
                        trace = F,
                        ...)

  predictions <- predict(model, newdata = test_data)
  actual <- data[test_indices, ][[all.vars(formula)[1]]]
  if (caret_metric =="RMSE") {
    metric <- sqrt(mean((predictions - actual)^2))
  } else if (caret_metric == "Kappa") {
    actual <- test_data[[all.vars(formula)[1]]]
    metric <- sum(predictions == actual) / length(actual)
  } else {
    stop("Unsupported data type for caret")
  }
  return(metric)
}





















