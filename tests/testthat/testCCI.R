# # Import package from GitHub
# devtools::install_github("ChristianBHT/CIsimdata")
# library(CIsimdata)
# 
# # Test script for the CCI package
# set.seed(1984)
# NormalData <- NormalData(400)
# # usethis::use_data(NormalData, overwrite = TRUE)
# 
# set.seed(1994)
# PolyData <- PolyData(600)
# # usethis::use_data(PolyData, overwrite = TRUE)
# 
# set.seed(1)
# UniformNoise_large <- UniformNoise(20000)
# # usethis::use_data(UniformNoise_large, overwrite = TRUE)
# 
# set.seed(1814)
# PoissonNoise <- PoissonNoise(1000)
# # usethis::use_data(PoissonNoise, overwrite = TRUE)
# 
# set.seed(1969)
# ExponentialNoise <- ExponentialNoise(1000)
# # usethis::use_data(ExponentialNoise, overwrite = TRUE)
# 
# set.seed(1914)
# NonLinNormal <- NonLinNormal(500)
# # usethis::use_data(NonLinNormal, overwrite = TRUE)
# 
# NonLinNormalZs_d0 <- NonLinNormalZs(N = 1000, d = 0, Zs = 15)
# # usethis::use_data(NonLinNormalZs_d0, overwrite = TRUE)
# 
# NonLinNormalZs_d05 <- NonLinNormalZs(N = 1000, d = 0.5, Zs = 15)
# # usethis::use_data(NonLinNormalZs_d05, overwrite = TRUE)

devtools::check()
devtools::check_win_devel()
devtools::document()
devtools::clean_dll()
devtools::build(path = "C:/Users/chris/Documents/GitHub/CCI")

devtools::install()
devtools::load_all()
library(CCI)
library(CIsimdata)
citation("CCI")


#-------------------------------------------------------------------------------
# Basic tests CCI.test()
#-------------------------------------------------------------------------------
dat <- NormalData(100)
result <- CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, nrounds = 500)
summary(result)

data <- ExpLogData(500)
data$Y <- as.factor(data$Y)
data$X <- as.factor(data$X)
result <- CCI.test(formula = Y ~ X + Z1 + Z2, data = data)
summary(result)

dat <- NormalData(100)
result <- CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, method = "xgboost")
summary(result)
data <- ExpLogData(500)
data$Y <- as.factor(data$Y)
data$X <- as.factor(data$X)
result <- CCI.test(formula = Y ~ X + Z1 + Z2, data = data, method = "xgboost")
summary(result)


dat <- NormalData(100)
result <- CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, method = "svm")
summary(result)
data <- ExpLogData(500)
data$Y <- as.factor(data$Y)
data$X <- as.factor(data$X)
result <- CCI.test(formula = Y ~ X + Z1 + Z2, data = data, method = "svm")
summary(result)

dat <- NormalData(400)
result <- CCI.test(formula = Y ~ X + Z1 , 
                   data = dat,
                   nperm = 200,
                   p = 0.7,
                   nrounds = 700,
                   metric = "RMSE",
                   choose_direction = TRUE,
                   poly = TRUE,
                   degree = 5,
                   seed = 1984,
                   subsample = 0.9,
                   parametric = TRUE,
                   verbose = T)
summary(result)

result <- CCI.test(formula = Y ~ X + Z1 , 
                   data = dat,
                   nperm = 100,
                   p = 0.7,
                   nrounds = 500,
                   method = "xgboost",
                   choose_direction = TRUE,
                   poly = TRUE,
                   degree = 5,
                   seed = 1984,
                   subsample = 0.9,
                   parametric = TRUE,
                   verbose = T,
                   progress = T,
                   min_child_weight = 3,
                   max_depth = 9,
                   eta = 0.2,
                   colsample_bytree = 0.2)
summary(result)

result <- CCI.test(formula = Y ~ X + Z1 , 
                   data = dat,
                   nperm = 100,
                   p = 0.7,
                   nrounds = 500,
                   method = "svm",
                   choose_direction = TRUE,
                   poly = TRUE,
                   degree = 5,
                   seed = 1984,
                   subsample = 0.9,
                   parametric = TRUE,
                   verbose = T,
                   progress = T,
                   min_child_weight = 3,
                   max_depth = 9,
                   eta = 0.2,
                   colsample_bytree = 0.2)

#-------------------------------------------------------------------------------
# Basic tests QQplot
#-------------------------------------------------------------------------------
dat <- NormalData(100)
cci_obj <- CCI.test(formula = Y ~ X + Z1 + Z2, data = dat, nperm = 500)
QQplot(cci_obj)


#--------------------
# With tuning
#--------------------

data <- NonLinNormal(200)
result <- CCI.test(formula = Y ~ X + Z1, interaction = F, nperm = 60, data = dat, method = 'xgboost', tune = TRUE)


#-------------------------------------------------------------------------------

Rsquare_metric  <- function(actual, predictions) {
  sst <- sum((actual - mean(actual))^2)
  ssr <- sum((actual - predictions)^2)
  metric <- 1 - (ssr / sst)
  return(metric)
}
CCI.test(formula = Y ~ X | Z2, data = dat, method = "rf", metricfunc = Rsquare_metric, tail = "right", seed = 2)
summary(result)

plot(result)

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





















