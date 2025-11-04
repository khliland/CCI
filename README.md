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
install.packages("CCI")
library(CCI)
```

## 1. Basic Usage

### Data

In order to show the functionality of the CCI package we will generate various different synthetic data sets 
that can be used to test conditional independence. We will define functions for this purpose as we go. 

The function below generates a dataset where `Y` is conditionally independent of `X` given `Z1` and `Z2`. The functions are linear 
so it should be easy to test conditional independence using traditional statistical tests as well.
```r
NormalData <- function(N){
  Z1 <- stats::rnorm(N,0,1)
  Z2 <- stats::rnorm(N,0,1)
  X <- stats::rnorm(N, Z1 + Z2, 1)
  Y <- stats::rnorm(N, Z1 + Z2, 1)

  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}
```
### Testing Conditional Independence
Below is an example of the simplest line of code needed to test the hypothesis `Y _||_ X | Z1, Z2` using the CCI package.
The code generates a dataset with 400 observations, where `Y` is conditionally independent of `X` given `Z1` and `Z2`.
The main user interface function for CCI is CCI.test(), which must at least have a formula and a data frame as input to perform a test.
```r
set.seed(1985)
data <- NormalData(400)
t <- CCI.test(formula = Y ~ X | Z1 + Z2, data = data)
summary(t)
```
The output for this test should look something like this:

**Computational Conditional Independence Test
**--------------------------------------------
**Method:    CCI test using rf 
**Formula:   Y ~ X | Z1 + Z2 
**Permutations:  60 
**Metric:    RMSE 
**Tail:      left 
**Statistic: 1.144 
**P-value:   0.9344 


The output tells us that you have performed a computational test of conditional independence using Random Forest  (`rf`) as the base machine learning algorithm, testing the condition `Y ~ X | Z1 + Z2`.
The performance metric used to build the null distribution and the test statistic is Root Mean Square Error (RMSE), indicating that `Y` is a continuous variable.
The test statistic is 1.144, and the p-value is 0.9344, therefor we fail to reject the null hypothesis at a significance level of 0.05. Which is correct.
tail is set to "left", meaning that the further left the test statistic is in the null distribution, the lower the p-value, this is not important now, but will be important when we use custom performance metrics later.

We can plot the null distribution by running `plot()` in a CCI object.
```r
plot(t)
```
In the previous example we tested a true null hypothesis, for completeness, we will now test a false null hypothesis, where `Y` is not conditionally independent of `X` given `Z1`.
```r
summary(CCI.test(formula = Y ~ X | Z1, data = data, parametric = TRUE))
```
The output of the last test should look something like this: 

Computational Conditional Independence Test
--------------------------------------------
Method:    CCI test using rf 
Formula:   Y ~ X | Z1 
Permutations:  60 
Metric:    RMSE 
Tail:      left 
Statistic: 1.293 
P-value:   0.003011 

At significant level 0.05 the test rejects the null hypothesis of `Y ~ X | Z1`, since the p-value is less than 0.05.
We also added in the argument parametric = TRUE, which means that the p-value is calculated assuming that the null distribution is Gaussian.
The default way of calculating a p-value is empirically, which means that the p-value is calculated by comparing the test statistic to the null distribution.
Using a parametric p-value is usually a good idea, since the empircal derived p-value is limited in how small it can be. 

## 2. Handling Different Data Types
One of the main motivation for having a computational test of conditional independence is to be able to test conditional independence using different data types, such as continuous, binary, and categorical data.
Depending on the data type of `Y` in the formula `Y ~ X | Z1 + Z2`, CCI adjusts the `metric` argument to `"RMSE"` (default) or `"Kappa"`, however, you can also do it manually. 
If the dependent variable `Y` in the formula `Y ~ X | Z1 + Z2` is continuous, then the performance metric used is RMSE (Root Mean Square Error). 
If it is a factor variable or binary variable, then the performance metric used is Kappa score.

In the example below, both `Y` and `X` are binary variables, meaning they only take on values 0 and 1. 
```r
BinaryData <- function(N, threshold = 0) {
  Z1 <- stats::rnorm(N)
  Z2 <- stats::rnorm(N)
  X <- ifelse(stats::rnorm(N, Z1 + Z2 + Z1*Z2, 1) < threshold, 1, 0)
  Y <- ifelse(stats::rnorm(N, Z1 + Z2 + Z1*Z2, 1) < threshold, 1, 0)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}
```


```r
set.seed(1985)
data <- BinaryData(500)
cci_test <- CCI.test(formula = Y ~ X | Z1 + Z2, data = data, metric = "Kappa")
summary(cci_test)
plot(cci_test)
```
If you notice in the example above, the null distribution looks kinda weird. It consist of two peaks, one at around -0.5 and one at 0.5. 
This is in general not a good sign, and indicate that you should try to change the method.
The CCI package comes with three build in machie learning methods for testing, Random Forest 'rf', Extreme Gradient Boosting 'xgboost' and Support Vector Machines 'svm'.
If Random Forest fails, one should can change the method to XGBoost, like this: 
```r
test_w_xgb <- CCI.test(formula = Y ~ X | Z1 + Z2, data = data, metric = "Kappa", method = "xgboost")
summary(test_w_xgb)
plot(test_w_xgb)
```
Viola, now the null distribution looks much better, which is what we want. The p-value is 0.75 which is ok since `Y` and `X` are by construction conditionally independent given `Z1` and `Z2`.

The third available method is support vector machine (`svm`). `svm` is very fast in small and medium data sets, 
but not as robust as `rf` and `xgboost`.
```r
set.seed(1985)
data <- BinaryData(500)
test_w_svm <- CCI.test(formula = Y ~ X | Z1 + Z2, data = data, metric = "Kappa", method = "svm")
summary(test_w_svm)
plot(test_w_svm)
```
When you are dealing with character variables representing categories, you can convert these variables into factor variable. 
'CCI.test' use metric = "Kappa", automatically.

Let us create a somewhat complicated data generating function for categorical data, 
```r
ComplicatedData <- function(N) {
  Z1 <- stats::runif(N, -1, 1)
  Z2 <- stats::runif(N, -1, 1)

  X <- sin(Z1 * pi) + Z2 + stats::rnorm(N, 0, 0.1)

  Y <- ifelse(cos(Z1 * pi) + Z2 > 1, "Laptop",
              ifelse(cos(Z1 * pi) + Z2 > 0.5, "Desktop",
                     ifelse(cos(Z1 * pi) + Z2 > 0, "GamePad", "Phone")))


  return(data.frame(Z1, Z2, X, Y))
}
```
Let's test a false hypothesis where `Y` is not conditionally independent of `X` given `Z1`.
```r
set.seed(1945)
data <- ComplicatedData(1000)
data$Y <-as.factor(data$Y) # Make sure Y is a factor variable
factor_test <- CCI.test(formula = Y ~ X | Z1, data = data)
summary(factor_test)
```
Let's see how `svm` performes
```r
test_svm_factor <- CCI.test(formula = Y ~ X | Z2, data = data, method = "svm", nperm = 150, seed = 1)
summary(test_svm_factor)
```
In this case both methods successfully reject the null hypothesis which is what we want. 
In the last example we set the argument `nperm = 150`, which means that the null distribution is generated using 150 Monte Carlo samples. 
The default `nperm` value is 60, which will usually be enough. In some cases increasing the number of samples to get a more precise null distribution at the cost of longer computation time.
We also set the seed parameter to 1, which ensures that the random number generation is reproducible. In everyday testing, setting the seed is a good idea. 

So now we have handled the `metric` and `nperm` arguments. An important argument in `CCI.test()` is the `p` argument, which controls the proportion of data used for training the model in each Monte Carlo Sample.
Default value is 0.5, but it is often a good idea to increase this value to 0.7 or 0.8.
In large data sets, you can set `p` to a lower value, like 0.1, to speed up the process and increase precision.
```r
SineGaussian <- function(N, a = 1, d = 0){
  Z = stats::rnorm(N,0,1)
  X = exp(-(Z)^2 / 2) * sin(a * (Z)) + 0.3*stats::rnorm(N,0,0.1)
  Y = exp(-(Z)^2 / 2) * sin(a * (Z)) +  d*X + 0.3*stats::rnorm(N,0,0.1)
  df <- data.frame(Z,X,Y)
  return(df)
}
```
```r
set.seed(1986)
data <- SineGaussian(800, d = 0.5) # d = 0.5 breaks conditional independence Y _||_ X | Z
summary(CCI.test(formula = Y ~ X | Z, p = 0.8, nrounds = 1000, data = data, parametric = T, seed = 3))
```
There is also a `subsampling` argument which can be set to a value between 0 and 1. This reduces the sample size used for testing in each iteration, speeding up testing. 
In the example below, even with 2 million observations testing is "fast"" because we only use 0.1% of the data for training the model in each iteration.
```r
set.seed(1984)
data <- SineGaussian(2000000, d = 0.5) 
summary(CCI.test(formula = Y ~ X | Z, data = data, parametric = T, nperm = 100, p = 0.25, subsample = 0.001))
```

You can pass on arguments to the machine learning algorithm used in `CCI.test()`. Here is an example of using the `xgboost` method with custom parameters.
```r
UniformNoise <- function(N) {
  Z1 = stats::rnorm(N, 0, 1)
  Z2 = stats::rnorm(N, 0, 1)
  X = Z2 - Z1 - Z2 * Z1 + stats::runif(N, min=-2, max=2)
  Y = Z2 + Z1 + Z2 * Z1 + stats::runif(N, min=-2, max=2)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}
```

```r
set.seed(1066)
dat <- UniformNoise(N = 2000)
complex_test <- CCI.test(formula = Y ~ X | Z2, # Non independence
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
summary(complex_test)
```
### The formula
As you might have guessed, the formula argument in `CCI.test()` gives the conditional independent statement to test. 
The condition Y _||_ X | Z1, Z2 is written as `Y ~ X | Z1 + Z2` (alternatively as `Y ~ X + Z1 + Z2`, both is accepted). 
When `Y` is the dependent variable it means that `Y` is predicted while `X` is permuted to see if it improves predicition performance beyond `Z1` and `Z2`.
Naturally, the condition Y _||_ X | Z1, Z2 can also be tested by the formula `X ~ Y | Z1 + Z2`. So which way to choose?
Generally it is best to predict the variable with the best predictive performance. 
`CCI.test()` will automatically choose the variable with the best predictive performance if you set the argument `choose_direction = TRUE`. 
```r
NonLinNormal <- function(N){
  Z1 <- stats::rnorm(N,0,1)
  Z2 <- stats::rnorm(N,0,1)
  X <-  stats::rnorm(N,Z1*Z2,1)
  Y <-  stats::rnorm(N,exp(Z1*Z2),1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}
```

```r
set.seed(1815)
data <- NonLinNormal(N = 600)
summary(CCI.test(formula = Y ~ X | Z1, method = 'rf', data = data, nrounds = 800, choose_direction = TRUE, parametric = T, seed = 1985))
```
The output from the test:
Computational Conditional Independence Test
--------------------------------------------
**Method:    CCI test using rf 
**Formula:   X ~ Y | Z1 
**Permutations:  60 
**Metric:    RMSE 
**Tail:      left 
**Statistic: 1.347 
**P-value:   6.509e-05 
Even though we input the formula as `Y ~ X | Z1`, the function automatically changed it to `X ~ Y | Z1` since `X` had better predictive performance than `Y` given `Z1`.
```r
summary(CCI.test(formula = Y ~ X | Z1, method = 'rf', data = data, nrounds = 800, parametric = T, seed = 1985))
```
Not including choose direction gives:
Computational Conditional Independence Test
--------------------------------------------
**Method:    CCI test using rf 
**Formula:   Y ~ X | Z1 
**Permutations:  60 
**Metric:    RMSE 
**Tail:      left 
**Statistic: 35.38 
**P-value:   0.1701 
Not setting 'choose_direction = TRUE' gives a different result, failing to reject the null hypothesis. Should you then always set 'choose_direction = TRUE'?
choose_direction = TRUE is a good idea in many cases, however, it relies on `Y` and `X` being the same data type. In some cases, for instance with different data types, 
it might be better to test both directions manually and see if they agree.

## 2.1. QQ-plots of p-values
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

Next we will show further functionality in CCI.test() 

## 3. Testing CI in dagitty DAGs

Conditional independence testing is central in validating a causal model with the data. 
In the example below we load data from the study, we use the dagitty package to draw out a causal model extract 
conditional independence statements. reclassifying them to formulas, and loop through the formulas list using parallel processing. 

First we create some data
```r
dag_data <- function(N) {
  X1 = stats::rnorm(N, 0, 1)
  X2 = stats::rnorm(N, X1, 1)
  X3 = X1 + stats::runif(N, min=-2, max=2)
  X4 = stats::rnorm(N, X2*X3, 1)
  X5 = cos(X2) + sin(X3) + stats::runif(N, min=-2, max=2)
  X6 = X1 + X5 + stats::rnorm(N, 0, 1)
  df <- data.frame(X1, X2, X3, X4, X5, X6)
  return(df)
}
```

```r
data <- dag_data(1000)
library(dagitty)
# Define the DAG, but with missing path from X1 to X6
DAG <- dagitty('dag{X1 -> X2
     X1 -> X3
     X2 -> X4
     X2 -> X5
     X3 -> X4
     X3 -> X5
     X5 -> X6
}')
# Plot the DAG
plot(DAG)
# Run CCI test
conditions <- impliedConditionalIndependencies(DAG)
# Create formula list from conditions 

formulas <- list()
for (i in 1:length(conditions)) {
  cond <- conditions[[i]]
  lhs <- cond$X
  rhs <- cond$Y
  set <- cond$Z
  if (length(set) == 0) {
    # No conditioning set
  } else {
    formula <- as.formula(paste(lhs, "~", rhs, "|", paste(set, collapse = " + ")))
  }
  
  formulas[[i]] <- formula
}

results <- sapply(X = 1:length(formulas), function(i) {
p <- CCI.test(formula   = formulas[[i]],
    data      = data, 
    parametric = TRUE,
    seed      = i
  )$p.value
  
  data.frame(
    formula = deparse(formulas[[i]]),
    p_value = p)
})
results_df <- do.call(rbind, results)
print(results_df)
```
The results looks something like this:
      [,1]                  
 [1,] "X1 ~ X4 | X2 + X3"   
 [2,] "0.801104088237935"   
 [3,] "X1 ~ X5 | X2 + X3"   
 [4,] "0.573497167385236"   
 [5,] "X1 ~ X6 | X5"        
 [6,] "9.0489090219566e-27" 
 [7,] "X1 ~ X6 | X2 + X3"   
 [8,] "0.00028052810872208" 
 [9,] "X2 ~ X3 | X1"        
[10,] "0.254112291902073"   
[11,] "X2 ~ X6 | X5"        
[12,] "7.58983980211691e-13"
[13,] "X3 ~ X6 | X5"        
[14,] "9.58536122533411e-08"
[15,] "X4 ~ X5 | X2 + X3"   
[16,] "0.447290410293313"   
[17,] "X4 ~ X6 | X5"        
[18,] "0.795932362518178"   
[19,] "X4 ~ X6 | X2 + X3"   
[20,] "0.764899344816845"  

Testing reveals that the rejected null hypothesis at significance level 0.05 is `X1 ~ X6 | X5`, `X1 ~ X6 | X2 + X3`, `X2 ~ X6 | X5` and `X3 ~ X6 | X5`. 
The conditions `X2 ~ X6 | X5` and `X3 ~ X6 | X5` are rejected since `X1` is a common cause of `X2`, `X3` and `X6`.


## 6. Using the `mlfunc` and `metricfunc` arguments 
Advance users can pass on custom machine learning wrapper functions and performance metrics to the `CCI.test()` function using the `mlfunc` and `metricfunc` arguments, respectively.
This is intended so that you can use a machine learning algorithm or performance metric of your choice, allowing for greater flexibility and customization in your conditional independence testing.
The function passed through the `mlfunc` argument should take the following inputs: `formula`, `data`, `train_indices`, `test_indices` and `...`
- **`formula`**: The formula for the model.
- **`data`**: The dataset used for training and testing.
- **`train_indices`**: Indices for the training data.
- **`test_indices`**: Indices for the test data.
- **`...`**: is a place holder for any arguments passed to the ML algorithm.

The function should return a numeric value representing the model's performance. 

Here's the general structure of the wrapper functions in pseudo code :

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
Let's take a look at two examples, the first example uses the `nnet` package to create a neural network wrapper function.


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
summary(CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, tail = 'left', mlfunc = neuralnet_wrapper, nperm = 100, size = 10, decay = 0.1, maxit = 200, seed = 1))
```
In general we wouldn't recommend using neural networks for CCI testing, since they are hard to train, but this is just an example of how to create a custom wrapper function.

As a more useful example, we create a wrapper for using the caret package. The caret package contains functionality for over 200 machine learning algorithms. 
```r
library(caret)
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
```

```r
# Testing using k-nearest neighbors from caret
dat <- NonLinNormal(2000)
summary(CCI.test(formula = Y ~ X | Z1, data = dat, tail = 'left', mlfunc = caret_wrapper, caret_method = "knn", caret_data_type = "continuous", nperm = 100, seed = 2))
```

## 7. ️ Custom Performance Metric with `metricfunc`

The CCI package provides default performance metrics, such as RMSE for continuous outcomes and Kappa scores for binary and categorical outcomes. However, if you have a specific performance metric in mind, you can easily define your own using the `metricfunc` argument.

Your custom function should take the following inputs:

- **`actual`**: The true values 
- **`predictions`**: Predicted values
- **`...`**: Additional arguments if needed

The output should be a numeric value representing the performance metric. Here’s an example that calculates the \(R^2\) metric using `rf`:
```r
Rsquare_metric  <- function(actual, predictions) {
  sst <- sum((actual - mean(actual))^2)
  ssr <- sum((actual - predictions)^2)
  metric <- 1 - (ssr / sst)
  return(metric)
}
results <- CCI.test(formula = Y ~ X | Z2, data = dat, method = "rf", metricfunc = Rsquare_metric, tail = "right", seed = 2, verbose = TRUE)
```
Setting `verbose = TRUE` will print out the P-value after testing.

**Important:** When using a custom performance metric, you should also specify the `tail` argument:
- **`tail = "right"`**: Use if higher metric values indicate better model performance.
- **`tail = "left"`**: Use if lower metric values indicate better model performance.


### Time series data
CCI can also be used to test whether two time series variables are conditionally independent. However, analyzing time series data with CCI requires a relatively large data set due to the complexity and dependencies inherent in time series analysis. 
In this example we test if two (\(X\) and \(Y\) trends which diverge in a times series are conditional independent given previous lags of X. We then to the same for lags of Y to show that this test is rejected (using significance level = 0.05). 

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
summary(CCI.test(formula = X ~ Y | X_lag1 + Time, data = data, p = 0.7, nperm = 100, method = "xgboost", parametric = TRUE))
summary(CCI.test(formula = Y ~ X | X_lag1 + X_lag2 + Time, p = 0.7, nperm = 100, data = data, method = "xgboost", parametric = TRUE))

data$Y_lag1 <- c(NA, data$Y[-length(data$Y)])  
data$Y_lag2 <- c(NA, NA, data$Y[-(length(data$Y)-1):-(length(data$Y))])
summary(CCI.test(formula = Y ~ X | Y_lag1 + Y_lag2 + Time, p = 0.7, nperm = 100, data = data, method = "xgboost", parametric = TRUE))
```















