# Welcome to the CCI Package

Thanks for checking out the CCI (Computational Conditional Independence) package in R. The CCI-test conducts a computational test whether two variables are conditionally independent.

The test is based on predictive machine learning methods in combination with permutation and Monte Carlo Cross-Validation (MCCV), building an empirical null distribution and estimating a test statistic. Testing conditional independence is especially useful in causal inference modeling.

### Key Features:
- Generates null distributions and test statistics using permutation and MCCV.
- Computes p-values and provides visualization of distributions.
- Supports the machine learning algorithms: linear models (`lm`), random forests, and gradient boosting (`xgboost`).
- Allows for custom machine learning functions and performance metrics.

## Installation
You can install the development version of `CCI` from GitHub:
```r
install.packages("devtools")
devtools::install_github("khliland/CCI")
library(CCI)
```

## 1. Basic Usage

### Simulating Data

First, define functions to simulate different types of data. In all the simulate data functions 'X' and 'Y' are independent by construction only when conditioned on both 'Z1' and 'Z2', conditioning on either one alone is not sufficient. 

```r
NormalData <- function(N){
  Z1 <- rnorm(N, 0, 1)
  Z2 <- rnorm(N, 0, 1)
  X <- rnorm(N, Z1 + Z2, 1)
  Y <- rnorm(N, Z1 + Z2, 1)
  return(data.frame(Z1, Z2, X, Y))
}

BinaryData <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)
  X <- ifelse(rnorm(N, Z1 + Z2 + Z1 * Z2, 1) < 0, 1, 0)
  Y <- ifelse(rnorm(N, Z1 + Z2 + Z1 * Z2, 1) < 0, 1, 0)
  return(data.frame(Z1, Z2, X, Y))
}

TrigData <- function(N) {
  Z1 <- runif(N, -pi, pi)
  Z2 <- rnorm(N)
  X <- numeric(N)
  Y <- numeric(N)
  
  for (i in 1:N) {
    X[i] <- ifelse(sin(Z1[i]) + cos(Z2[i]) > 1, 3,
                   ifelse(sin(Z1[i]) + cos(Z2[i]) > 0, 2,
                          ifelse(sin(Z1[i]) > -1, 1, 0)))
    
    Y[i] <- ifelse(cos(Z1[i]) - sin(Z2[i]) > 1, 3,
                   ifelse(cos(Z1[i]) - sin(Z2[i]) > 0, 2,
                          ifelse(cos(Z1[i]) > -1, 1, 0)))
  }
  return(data.frame(Z1, Z2, X, Y))
}

NonLinNormal <- function(N){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = exp(Z1*Z2) + rnorm(N,0,1)
  Y <- Z1*Z2 + rnorm(N,0,1)
  return( data.frame(Z1,Z2,X,Y))
}

SinusoidalData <- function(N, a = 1){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  Z <- Z1 + Z2
  X = exp(-(Z)^2 / 2) * sin(a * (2*Z1 + 0.1*Z2)) + rnorm(N,0,0.1)
  Y = exp(-(Z)^2 / 2) * sin(a * (2*Z2 + 0.1*Z1)) + rnorm(N,0,0.1)
  return(data.frame(Z1,Z2,X,Y))
}
```

### Testing Conditional Independence
Below is an example of the simplest line of code needed to test the hypothesis `Y _||_ X | Z1, Z2` using the CCI package.


```r
set.seed(1985)
dat <- NormalData(400)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat)
CCI.test(formula = Y ~ X | Z1, data = dat, parametric = TRUE)
```
The output of the last test should look something like this: 

- **Computational conditional independence test using 'rf'** 
- **CI Condition Tested**: `Y ~ X | Z1` 
- **Number of Monte Carlo Samples**: 500
- **Performance Metric**: Root Mean Square Error (RMSE)
- **Test Statistic**: 1.230432
- **P-value**: 0.01594247
- **Tail**: Left-tailed test
- **Null Distribution Summary**: Mean = 1.461339, SD = 0.1076059
- **Visualization**: A plot of the null distribution and test statistic has been generated.

At significant level 0.05 the test rejects the null hypothesis of `Y ~ X | Z1`, since the p-value is less than 0.05. The CCI.test automatically generates a histogram over the null distribution and the corresponding test statistic. When the `parametric` argument is set to `TRUE`, the method assumes that the null distribution is approximately Gaussian.

## 2. Handling Different Data Types

Depending on the data type of `Y` in the formula `Y ~ X | Z1 + Z2`, you can adjust the `data_type` parameter to `"continuous"` (default), `"binary"`, or `"categorical"`. Here is an example with binary data:

```r
set.seed(1985)
dat <- BinaryData(500)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = "binary")
CCI.test(formula = Y ~ X | Z2, data = dat, data_type = "binary", method = "xgboost")
```

In the second example, we use `xgboost` as the machine learning algorithm. The CCI package offers three built-in methods: `lm` (linear model), `rf` (random forest, the default), and `xgboost` (extreme gradient boosting). While random forest is the default due to its balance of speed and accuracy, `xgboost` is more robust and recommended especially for binary and categorical data types. Custom machine learning algorithms can also be defined by the user. 

Testing conditional independence with categorical data types is difficult. The CCI-package can handle such cases,  but requires quit large data sets, again, we recommend to use `xgboost`. Unfortunately, it is also a little slow.

```r
set.seed(2097)
dat <- TrigData(1000)
dat$Y <- dat$Y - 1 # Xgboost requires that a categorical outcome 'starts' on 0
CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = 'categorical', method = "xgboost", num_class = 3)
# Note that we need to set the num_class argument for the xgb.train()
CCI.test(formula = Y ~ X | Z1, data = dat, data_type = 'categorical', method = "xgboost", num_class = 3)
```

Random forest `rf` is fast, however, you can switch to a linear parametric model which is even faster, though potentially less precise. If we can assume that the regression 'Y ~ X + Z1 + Z2' is well estimated by a parametric model, `lm` is a good choice. When using the `lm` method, you must also define the family argument as required by glm(). The exception is when `data_type` is `categorical` and `method` is `lm`. Since `lm` is much faster, we generate 2000 Monte Carlo samples for the null distribution. Here’s how to do it:

```r
set.seed(1984)
normal_dat <- NormalData(200)
trig_dat <- TrigData(200)

CCI.test(formula = Y ~ X | Z1 + Z2, data = normal_dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T)
CCI.test(formula = Y ~ X | Z1 + Z2, data = trig_dat, nperm = 2000, method = 'lm', data_type = "categorical", parametric = T)
CCI.test(formula = Y ~ X | Z2, data = normal_dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T)

```
In any statistical test, it might be the case that we have insufficient power and therefor one can not rely on one single p-value. An extra level of analysis in CCI is to create quantile-quantile (qq) plots over p-values, to see if they approximately follows a uniform distribution, here is how to do it.

```r
set.seed(1985)
dat <- NormalData(100)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T)
CCI.test(formula = Y ~ X | Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T) # Fail to reject null

test <- CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T, plot = F)
QQplot(test) # P-values roughly follow the diagonal line

test <- CCI.test(formula = Y ~ X | Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T, plot = F)
QQplot(test) # Distinct pattern of a skewed distribution of p-values, which make us reject the null
```
Note that assessing a qq plot is not a statistical test, and each test must be judge subjectively. 

These examples show the basics of comuptational testing of conditional independence with the CCI-package. Next we will show the various arguments in CCI.test() 

## 3. Testing CI in dagitty DAGs

As we have seen above, the absolute bare minimum arguments which need to be provided are `formula` and `data`. The formula must be of class formula and of the form Y ~ X + V + Z + ... etc or Y ~ X | V + Z + ... etc for testing the condition Y _||_ X | V, Z, ... .

The `dag` argument in `CCI.test()` can take a `dagitty` class object, representing a Directed Acyclic Graph (DAG). DAGs often contain multiple testable conditional independence statements. The specific statement you want to test is determined by the `dag_n` argument (with the default being `1`). When `dag` is define, we do not need to specify `formula`. Here’s an example:


```r
library(dagitty)

set.seed(1984)
data <- NonLinNormal(500)

dag <- dagitty('dag {
  X
  Y
  Z1
  Z2
  Z1 -> X
  Z1 -> Y
  Z2 -> X
  Z2 -> Y
}')

result <- CCI.test(formula = NULL,
                   data = data,
                   dag = dag,
                   dag_n = 1,
                   parametric = T)
```
In this example:

- We create a DAG with four nodes: `X`, `Y`, `Z1`, and `Z2`.
- Edges represent causal relationships: `Z1` and `Z2` both influence `X` and `Y`.
- We then run `CCI.test()` to test the first conditional independence statement in the DAG (`dag_n = 1`).

### Speeding Things Up with the `p` Argument
The `p` argument controls the proportion of data used for training the model, with the default set to `0.8`. If you’re dealing with a large dataset, you might want to set `p` to a lower value, like `0.1`, to speed up the process and increase precision. Here's how you can do it:

```r
set.seed(1983)
dat <- SinusoidalData(20000)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, method = 'xgboost', parametric = T, p = 0.1)
CCI.test(formula = Y ~ X | Z1, data = dat, method = 'xgboost', parametric = T, p = 0.1)
```
In this case, only 10% of the data is used for training, making the test faster (although still slow) and potentially more precise

## 4.  The `nperm` Argument

The `nperm` argument controls the number of Monte Carlo samples used to generate the null distribution. Adjusting `nperm` can help speed up testing, but be aware that a lower number of samples might lead to less precise results. Here’s an example:

```r
set.seed(1983)
dat <- TrigData(1500)
dat$Y <- dat$Y - 1 

CCI.test(formula = Y ~ X | Z1, data = dat, method = 'xgboost', data_type = "categorical", num_class = 3, nperm = 100, parametric = TRUE)
```

In this example, we use only 100 permutations, which speeds up the test but might slightly reduce precision.

## 5. Combining `p` and `nperm` for Faster, Precise Testing

You can also combine the `p` and `nperm` arguments for both speed and precision. For instance, by using only one-third of the data for training (`p = 1/3`) and increasing the number of permutations to 300, you can create a robust null distribution while keeping the test efficient:

```r
set.seed(1983)
dat <- TrigData(1500)
dat$Y <- dat$Y - 1 
p = 1/3
CCI.test(formula = Y ~ X | Z1, data = dat, method = 'xgboost', data_type = "categorical", num_class = 3, p = p, nperm = 300, parametric = TRUE)
```
## 6. ️ Passing Other Arguments to the Machine Learning Function
When using different machine learning methods with `CCI.test()`, you can fine-tune the models by passing in additional arguments specific to the algorithm. For example, the `nrounds` argument controls the number of trees in `rf` (default is 120). You can also adjust parameters like `max.depth`, `min.node.size`, and `sample.fraction` to control the depth of the trees, minimum node size, and fraction of the data used, respectively.

In the example below, we set the number of trees to 100, limit the maximum depth of the trees to 6 (unlimited by default), reduce the minimum node size to 4 (default is 5), and use 70% of the data (default is 100%). These adjustments help speed up the estimation process:

```r
set.seed(1979)
dat <- NonLinNormal(1000)
CCI.test(formula = Y ~ X | Z2, data = dat, parametric = TRUE, nrounds = 100, max.depth = 6, min.node.size = 4, sample.fraction = 0.7)
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
neuralnet <- function(formula,
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
CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, mlfunc = neuralnet, nperm = 200, size = 10, decay = 0.1, maxit = 200, tail = "left")
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
### Example 3 


## 11. Methodology of the CCI Test

Here's how the CCI test works when testing the statement \( X \perp\!\!\!\perp Y \mid Z \):

1. **Permuting \(Y\)**: Start by permuting \(Y\) into \(Y^p\), which breaks any existing dependency between \(X\) and \(Y\).
2. **Subset Selection and Model Estimation**: Take a subset of the data of size \(p\) (where \(0 < p < 1\)) and estimate the relationship \(X = f(Y^p, Z)\) using this subset.
3. **Performance Evaluation**: Use the remaining \(1-p\) portion of the data to evaluate the performance of the predictions from \(X = f(Y^p, Z)\) using a performance metric like RMSE.
4. **Generating the Null Distribution**: Repeat the above steps (e.g., 500 times) to generate a null distribution of the performance metric under the assumption of conditional independence.
5. **Test Statistic Calculation**: Finally, calculate the test statistic by estimating the relationship \(X = f(Y, Z)\) using the original (non-permuted) data.

The observed test statistic is compared against the null distribution. If it falls far from the bulk of the null distribution, it suggests a strong relationship between \(X\) and \(Y\) given \(Z\), violating the null hypothesis of conditional independence.
