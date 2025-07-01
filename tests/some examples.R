# Some examples
library(CCI)

set.seed(1984)
dat <- NormalData(400)
example_1 <- CCI.test(formula = Y ~ X | Z1 + Z2, data = dat)
summary(example_1)
plot(example_1)

CCI.test(formula = Y ~ X | Z1,
         data = dat)
CCI.test(formula = Y ~ X | Z1,
         data = dat, parametric = T)

set.seed(1985)
dat <- BinaryData(500)
test<- CCI.test(formula = Y ~ X | Z1,
                data = dat,
                method = "xgboost",
                parametric = T)
summary(test)

set.seed(1979)
dat <- NonLinNormal(90)

cci_obj <- CCI.test(formula = Y ~ X | Z1,
                    data = dat,
                    nperm = 500,
                    parametric = T,
                    interaction = F)
QQplot(cci_obj)

set.seed(1066)
dat <- UniformNoise(20000)

CCI.test(
  formula = Y ~ X | Z2 ,
  data = dat,
  method = "xgboost",
  parametric = TRUE,
  p = 0.05,
  subsample = 0.7)

library(dagitty)
dag <- dagitty('dag {
  Z1 -> X
  Z1 -> Y
  Z2 -> X
  Z2 -> Y
}')
set.seed(1998)
data <- ComplexCategorization(600)
data$Y <- as.factor(data$Y)
data$X <- as.factor(data$X)

CCI.test(dag = dag,
         dag_n = 1,
         data = data,
         parametric = T,
         nperm = 150,
         method = 'rf')


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

set.seed(1814)
data <- PoissonNoise(1000)
CCI.test(formula = Y ~ X | Z1,
         data = data,
         mlfunc = caret_wrapper,
         caret_method = "treebag",
         caret_metric = "continuous",
         tail = "left",
         interaction = F,
         parametric = T)
warnings()
MedAE  <- function(actual, predictions, ...) {
  metric <- median(abs(actual - predictions))
  return(metric)
}

set.seed(1969)
data <- ExponentialNoise(1000)
CCI.test(formula = Y ~ X | Z1 + Z2,
         data = data,
         method = "rf",
         metricfunc = MedAE,
         tail = "left",
         parametric = T,
         nperm = 250)
