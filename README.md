# Welcome to the CCI Package

Thanks for checking out the development version of CCI (Computational Conditional Independence) package in R. 
The CCI-package is made for conducting computational testing for conditionally independent.
Conditional independence is written as Y _||_ X | Z, meaning that the random variable Y is independent of X given Z.
Computational testing of conditional independence is a machine learning based test where permutation and Monte Carlo Cross-Validation (MCCV) us used to build an empirical null distribution and estimating a test statistic. 


### Key Features:
In this readme we will go through all the available features of the CCI package, and provide heuristics for how to use it. 
The CCI package is designed to be flexible and hopefully user-friendly, allowing you to test conditional independence in various data types and scenarios. 
Report any bugs or issues you encounter, and feel free to contribute to the package on GitHub.
## Installation
You can install the development version of `CCI` from GitHub:
```r
install.packages("devtools")
devtools::install_github("khliland/CCI")
library(CCI)
```

## 1. Basic Usage

### Data

The CCI-package comes with several built-in data generating functions to help you get started. 
These functions generate synthetic data sets that can be used to test conditional independence. 

### Testing Conditional Independence
Below is an example of the simplest line of code needed to test the hypothesis `Y _||_ X | Z1, Z2` using the CCI package.
The code generates a dataset with 400 observations, where `Y` is conditionally independent of `X` given `Z1` and `Z2`.
The main user interface function for CCI is CCI.test(), which must at least have a formula and a data frame as input to perform a test.
```r
set.seed(1985)
data <- NormalData(400)
CCI.test(formula = Y ~ X | Z1 + Z2, data = data)
```
The output for this test should look something like this:

- **Computational conditional independence test using 'rf'.
- **Null hypothesis:  Y ~ X | Z1 + Z2 
- **Number of Monte Carlo samples:  60 
- **Performance Metric:  RMSE 
- **Test Statistic:  1.143737 
- **P-value:  0.9344262 
- **tail:  left 
- **Summary of Null Distribution: mean =  1.064956 , sd =  0.04709121 
- **Plot generated.

The output tells us that you have performed a computational test of conditional independence using the random forest method (`rf`), testing the condition `Y ~ X | Z1 + Z2`.
The performance metric used to build the null distribution and the test statistic is Root Mean Square Error (RMSE).
The test statistic is 1.143737, and the p-value is 0.9344262, indicating that we fail to reject the null hypothesis at a significance level of 0.05. Which is correct.
tail is set to "left", meaning that the further left the test statistic is in the null distribution, the lower the p-value.
The summary of the null distribution shows that the mean is 1.064956 and the standard deviation is 0.04709121.
The CCI package automatically generates a histogram over the null distribution and the corresponding test statistic, which can be useful for visualizing the results.

In the previous example we tested a true null hypothesis, for completeness, we will now test a false null hypothesis, where `Y` is not conditionally independent of `X` given `Z1`.
```r
CCI.test(formula = Y ~ X | Z1, data = data, parametric = TRUE)
```
The output of the last test should look something like this: 

- **Computational conditional independence test using 'rf'.
- **Null hypothesis:  Y ~ X | Z1 
- **Number of Monte Carlo samples:  60 
- **Performance Metric:  RMSE 
- **Test Statistic:  1.292519 
- **P-value:  0.003010517 
- **tail:  left 
- **Summary of Null Distribution: mean =  1.447802 , sd =  0.05653567 
- **Plot generated.

At significant level 0.05 the test rejects the null hypothesis of `Y ~ X | Z1`, since the p-value is less than 0.05.
We also added in the argument parametric = TRUE, which means that the p-value is calculated assuming that the null distribution is Gaussian.
The default way of calculating a p-value is empirically, which means that the p-value is calculated by comparing the test statistic to the null distribution.
Using a parametric p-value is usually a good idea, since the empircal derived p-value is limited in how small it can be. 

## 2. Handling Different Data Types
One of the main motivation for having a computational test of conditional independence is to be able to test conditional independence using different data types, such as continuous, binary, and categorical data.
Depending on the data type of `Y` in the formula `Y ~ X | Z1 + Z2`, you can adjust the `metric` argument to `"RMSE"` (default) or `"Kappa"`. 

In the example below, both `Y` and `X` are binary variables, meaning they only take on values 0 and 1. 

```r
set.seed(1985)
data <- BinaryData(500)
CCI.test(formula = Y ~ X | Z1 + Z2, data = data, metric = "Kappa")
```
If you notice in the example above, the null distribution looks kinda weird. It consist of two peaks, one at around -0.5 and one at 0.5. 
This is in general not a good sign, and indicate that you should try to change the method.
The two main method options in CCI.test() are 'rf' and 'xgbosst'. First we can change the method to XGBoost, like this: 
```r
set.seed(1985)
data <- BinaryData(500)
CCI.test(formula = Y ~ X | Z1 + Z2, data = data, metric = "Kappa", method = "xgboost")
```
Viola, now the null distribution looks much better, which is what we want. The p-value is 0.72 which is what we expect since `Y` and `X` are conditionally independent given `Z1` and `Z2`.

The third available method is support vector machine (`svm`). `svm` is fast, but not as robust as `rf` and `xgboost`.
```r
set.seed(1985)
data <- BinaryData(500)
CCI.test(formula = Y ~ X | Z1 + Z2, data = data, metric = "Kappa", method = "svm")
```
When `Y` is a factor variable,`CCI.test()` automatically handles categorical variables as factor variables. 
```r
set.seed(1945)
data <- TrigData(500)
data$Y <-as.factor(data$Y) # Make sure Y is a factor variable
data$X <-as.factor(data$X) # Make sure X is a factor variable
CCI.test(formula = Y ~ X | Z2, data = data)
```
For completeness, we also see how `svm` performes
```r
CCI.test(formula = Y ~ X | Z2, data = data, method = "svm", nperm = 250)
```
All three methods successfully reject the null hypothesis which is what we want. 
In the last example we set the argument `nperm = 250`, which means that the null distribution is generated using 250 Monte Carlo samples. 
The default `nperm` value is 60, which will usually be enough, but it does not hurt to increase the number of samples to get a more precise null distribution.

The last important argument in `CCI.test()` is the `p` argument, which controls the proportion of data used for training the model in each Monte Carlo Sample, default value is 0.5.
In lagrge datasets, you can set `p` to a lower value, like 0.1, to speed up the process and increase precision.
```r
set.seed(1984)
data <- SineGaussian(10000, d = 0.3) # d = 0.3 breaks conditional independence Y _||_ X | Z
CCI.test(formula = Y ~ X | Z, data = data, parametric = T, p = 0.05)
```
There is also a `subsampling` argument which can be set to a value between 0 and 1. This reduces the sample size used for testing in each iteration, speeding up testing. 
In the example below, even with 2 million observations testing is "fast"" because we only use 0.1% of the data for training the model in each iteration.
```r
set.seed(1984)
data <- sineGaussian(2000000, d = 0.3) # d = 0.3 breaks conditional independence Y _||_ X | Z
CCI.test(formula = Y ~ X | Z, data = data, parametric = T, nperm = 100, p = 0.25, subsample = 0.001)
```

Finally we show that you can pass on arguments to the machine learning algorithm used in `CCI.test()`. Here is an example of using the `xgboost` method with custom parameters.
```r
set.seed(1066)
dat <- UniformNoise(N = 2000)
CCI.test(formula = Y ~ X | Z2, 
data = dat, 
method = "xgboost", 
parametric = TRUE, 
p = 0.25,
nthread = 5, 
subsample = 0.7, 
alpha = 0.3, 
eta = 0.1, 
max_depth = 6, 
min_child_weight = 3, 
colsample_bytree = 0.8, 
gamma = 0.1, 
nrounds = 100, 
objective = "reg:pseudohubererror")
```

### The formula
As you might have guessed, the formula argument in `CCI.test()` gives the conditional independent statement to test. 
The condition Y _||_ X | Z1, Z2 is written as `Y ~ X | Z1 + Z2` (alternativly as `Y ~ X + Z1 + Z2`, both is accepted). 
When `Y` is the dependent variable it means that `Y` is predicted while `X` is permuted to see if it improves predicition performance beyond `Z1` and `Z2`.
Naturally, the condition Y _||_ X | Z1, Z2 can also be tested by the formula `X ~ Y | Z1 + Z2`. So which way to choose?
Generally it is best to predict the variable with the best predictive performance. 
`CCI.test()` will automatically choose the variable with the best predictive performance if you set the argument `choose_direction = TRUE`. 

```r
set.seed(1814)
data <- SineGaussianBiv(N = 1000, a = 1, d = 0.5)
CCI.test(formula = Y ~ X | Z1 + Z2, method = 'rf', data = data, nrounds = 800, choose_direction = TRUE, parametric = T)
```
```r
set.seed(1814)
data <- SineGaussianBiv(N = 1000, a = 1, d = 0)
CCI.test(formula = Y ~ X | Z1 + Z2, method = 'rf', data = data, parametric = T, nrounds = 1000, choose_direction = T)
```

In any statistical test, it might be the case that we have insufficient power and therefor one can not rely on one single p-value. 
An extra level of analysis in CCI, or any other statistical test, is to create quantile-quantile (qq) plots over p-values, to see if they approximately follows a uniform distribution, here is how to do it.

```r
set.seed(1985)
data <- NormalData(80)
CCI_obj <- CCI.test(formula = Y ~ X | Z1 + Z2, data = data, nperm = 200, parametric = T)
QQplot(CCI_obj) # P-values roughly follow the diagonal line
# Testing a false Null
CCI_obj <- CCI.test(formula = Y ~ X | Z2, data = data, nperm = 200, parametric = T)
QQplot(CCI_obj) # P-values do not follow diagonal line
```
Note that assessing a qq plot is not a statistical test, and each test must be judge subjectively. 

These examples show the basics of computational testing of conditional independence with the CCI-package. 

Next we will show the various functionality in CCI.test() 

## 3. Testing CI in dagitty DAGs

As we have seen above, the absolute bare minimum arguments which need to be provided are `formula` and `data`. 
Instead of the formula we can also test conditional independence statements directly from a dagitty DAG. 
For that we must use the `dag` argument in `CCI.test()`. 
The specific statement you want to test is determined by the `dag_n` argument (with the default being `1`). 
When `dag` is define, we do not need to specify `formula`, if you do, this will overwrite the `dag_n` argument. 

Here’s an example:

```r
library(dagitty)

set.seed(1984)
data <- NonLinNormal(500)

dag <- dagitty('dag {
  Z1 -> X
  Z1 -> Y
  Z2 -> X
  Z2 -> Y
}')

result <- CCI.test(data = data,
                   dag = dag,
                   dag_n = 1,
                   parametric = T)
```

## 6. Using the `mlfunc` and `metricfunc` arguments 
Advance users can pass om custom machine learning wrapper functions and performance metrics to the `CCI.test()` function using the `mlfunc` and `metricfunc` arguments, respectively.
This is intended so that you can use any machine learning algorithm or performance metric of your choice, allowing for greater flexibility and customization in your conditional independence testing.
The function passed through the `mlfunc` argument should take the following inputs: `formula`, `data`, `train_indices`, `test_indices` and `...`
```
my_CCI_wrapper <- function(formula, data, train_indices, test_indices,...) {
  train_data <- data[train_indices, ]
  test_data <- data[test_indices, ]
  model <- train_model(formula = formula, data = train_data, ...)
  predictions <- predict_model(model, newdata = test_data)
  actual <- get_true_values(formula, data = test_data)
  performance <- compute_metric(actual, predictions)
  return(performance)
}
```

By reducing the number of trees, limiting tree depth, and using a smaller sample fraction, you can significantly speed up the testing process, however, the settings on the machine learning function should ideally be determined by pre tuning. 

Using the `xgboost` option for method, you can pass in various arguments to "fine-tune" the performance of `xgboost`. For example:

```r
set.seed(69)
dat <- NonLinNormal(1000)
CCI.test(formula = Y ~ X | Z2, data = dat, method = "xgboost", parametric = TRUE, nthread = 5, subsample = 0.7, lambda = 0.8, objective = "reg:pseudohubererror", seed = 2)
```
In this example:

- **`nthread = 5`**: Uses 5 threads for parallel processing, hopefully speeding up computation (default = 1).
- **`subsample = 0.7`**: Uses 70% of the data for each boosting round, helping prevent overfitting and speed up computation.
- **`lambda = 0.8`**: Adds L2 regularization to the weights, making the model more robust.
- **`objective = "reg:pseudohubererror"`**: Changes the loss function to pseudohuber, which is more resistant to outliers than squared error (the default).

## 7. ️ Custom Performance Metric with `metricfunc`

The CCI package provides default performance metrics, such as RMSE for continuous outcomes and Kappa scores for binary and categorical outcomes. However, if you have a specific performance metric in mind, you can easily define your own using the `metricfunc` argument.

Your custom function should take the following inputs:

- **`data`**: The dataset used for the test.
- **`model`**: The trained model.
- **`test_indices`**: Indices for the test data.
- **`test_matrix`**: (For `xgboost` only) The matrix used for predictions.

The output should be a numeric value representing the performance metric. Here’s an example that calculates the \(R^2\) metric using `xgboost`:

```r
Rsquare_metric  <- function(data, model, test_indices, test_matrix) {
    actual <- data[test_indices,][['Y']]
    pred <- predict(model, newdata = test_matrix)
    sst <- sum((actual - mean(actual))^2)
    ssr <- sum((actual - pred)^2)
    metric <- 1 - (ssr / sst)
    return(metric)
}
                  
set.seed(1914)
dat <- NonLinNormal(500)
CCI.test(formula = Y ~ X | Z2, data = dat, method = "xgboost", metricfunc = Rsquare_metric, tail = "right")
```

**Important:** When using a custom performance metric, you must also specify the `tail` argument:
- **`tail = "right"`**: Use if higher metric values indicate better model performance.
- **`tail = "left"`**: Use if lower metric values indicate better model performance.

## 8. Custom Machine Learning Algorithm with `mlfunc`

You can also define a custom machine learning function using the `mlfunc` argument. This allows you to implement any algorithm of your choice, tailored to your specific needs. The custom function should take these inputs:

- **`formula`**: The formula for the model.
- **`data`**: The dataset used for training and testing.
- **`train_indices`**: Indices for the training data.
- **`test_indices`**: Indices for the test data.

The function should return a numeric value representing the model's performance. Here's an example using a neural network and RMSE for performance metric:

```r
neuralnet_wrapper <- function(formula,
                      data,
                      train_indices,
                      test_indices,
                      ...) {
                              
    model <- nnet::nnet(formula, data = data[train_indices, ], linout = TRUE, trace = FALSE, ...)
  
    predictions <- predict(model, newdata = data[test_indices, ])
    actual <- data[test_indices, ][[all.vars(formula)[1]]]

    metric <- sqrt(mean((predictions - actual)^2))

    return(metric)
}

dat <- NonLinNormal(2000)
CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, mlfunc = neuralnet_wrapper, nperm = 200, size = 10, decay = 0.1, maxit = 200, tail = "left")
```
```r
caret_wrapper <- function(formula,
                          data,
                          train_indices,
                          test_indices,
                          caret_method,
                          caret_data_type,
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
  if (caret_data_type =="continuous") {
    metric <- sqrt(mean((predictions - actual)^2))
  } else if (caret_data_type %in% c("categorical", "binary")) {
    actual <- test_data[[all.vars(formula)[1]]]
    metric <- sum(predictions == actual) / length(actual)
  } else {
    stop("Unsupported data type for wrapper")
  }
  return(metric)
}
}
```

## 10. More Examples

### Example 1 (Bagging tree)
Here’s an example of using the `CCI` test with custom data:

```r
PolyData <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)
  X <- numeric(N)
  Y <- numeric(N)

  for (i in 1:N) {
    X[i] <- ifelse(Z1[i]^2 + Z2[i]^2 > 2, 3,
                   ifelse(Z1[i]^2 + Z2[i] > 0.5, 2,
                          ifelse(Z1[i] + Z2[i]^2 > 0, 1, 0)))

    Y[i] <- ifelse(Z1[i]^3 + Z2[i] > 1, 3,
                   ifelse(Z1[i]^2 - Z2[i]^2 > 0, 2,
                          ifelse(Z1[i] - Z2[i]^3 > -1, 1, 0)))
  }

  return(data.frame(Z1, Z2, X, Y))
}

set.seed(100)
dat <- PolyData(931)
CCI.test(formula = X ~ Y + Z1, 
         data = dat, 
         data_type = "categorical",  
         method = "xgboost", 
         booster = "gbtree", 
         num_class = 4,
         max_depth = 6, 
         eta = 0.3, 
         subsample = 0.7,  
         colsample_bytree = 0.7)
```
In this example we use the bagging tree algorithm as implemented in the `xgboost` package.

### Example 2 (Custom multi class log loss with xgboost)
First we define a new data generating function.
```r
Multinominal <- function(N, zeta = 1.5) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)
  xb1 <- Z2 + zeta*Z1*Z2 + zeta*Z1
  xb2 <- Z2 - zeta*Z1
  xp1 <- 1/(1+exp(xb1) + exp(xb2))
  xp2 <- exp(xb1) /(1+exp(xb1) + exp(xb2))
  random <- runif(N,0, 1)
  X <- ifelse(random < xp1, "C", ifelse(random < xp1 + xp2,"A","B"))
  yb1 = zeta*Z1*Z2
  yb2 <- exp(Z2) +  zeta*Z1
  yp1 <- 1/(1+exp(yb1) + exp(yb2))
  yp2 <- exp(yb1) /(1+exp(yb1) + exp(yb2))
  random <- runif(N,0, 1)
  Y <- ifelse(random < yp1, "X", ifelse(random < yp1 + yp2,"Y","Z"))
  return(data.frame(Z1,Z2,X,Y))
}
```
```r
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

  metric <- CCI.test(formula = Y ~ X + Z1 + Z2,
                            data = data,
                            data_type = "categorical",
                            method = "xgboost",
                            nrounds = 120,
                            num_class = 3,
                            eta = 0.1,
                            lambda = 0.5,
                            alpha = 0.5,
                            metricfunc = multi_class_log_loss,
                            tail = "left")
```


### Example 3 (Time series data)
CCI can also be used to test whether two time series variables are conditionally independent. However, analyzing time series data with CCI requires a relatively large data set due to the complexity and dependencies inherent in time series analysis. In this example we test if two (\(X\) and \(Y\) trends which diverge in a times series are conditional independent given previous lags of X. We then to the same for lags of Y to show that this test is rejected (using significance level = 0.05). 

```r
time_series <- function(n, phi1, phi2) {
  phi <- c(phi1, phi2)
  # We generate X
  X <- arima.sim(n = n, list(ar = phi))
  Y <- numeric(n)
  
  for (t in 3:n) {
    Y[t] <- 0.01 * t +  1.2 * X[t-1] +  0.7 * X[t-2] +  0.5 * X[t-2]*X[t-1] + rnorm(1, sd = 1)
  }
  data <- data.frame(Time = 1:n, X = X, Y = Y)
}
set.seed(1993) 
data <- time_series(n = 1500, phi1 = 0.9, phi2 = -0.5)

data$X_lag1 <- c(NA, data$X[-length(data$X)])  
data$X_lag2 <- c(NA, NA, data$X[-(length(data$X)-1):-(length(data$X))])

cor(data$Y, data$X) # Showing the correlation between Y and X, indicating that they are not independent

# Inputing Time as a conditioning variables for general trends.
data <- na.omit(data)
CCI.test(formula = X ~ Y | X_lag1 + Time, data = data, p = 0.5, nperm = 200, method = "xgboost")
CCI.test(formula = Y ~ X | X_lag1 + X_lag2 + Time, p = 0.5, nperm = 200, data = data, method = "xgboost")

data$Y_lag1 <- c(NA, data$Y[-length(data$Y)])  
data$Y_lag2 <- c(NA, NA, data$Y[-(length(data$Y)-1):-(length(data$Y))])
CCI.test(formula = Y ~ X | Y_lag1 + Y_lag2 + Time, p = 0.5, nperm = 200, data = data, method = "xgboost", parametric = T)
```

















