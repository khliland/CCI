# Welcome to the CCI Package

Thanks for checking out the CCI (Computational Conditional Independence) package in R. The CCI test is designed to assess whether two variables are conditionally independent given a set of conditioning variables.

The package is designed to test for conditional independence between two variables given a set of conditioning variables. It utilizes machine learning models, permutation testing, and Monte Carlo cross-validation to estimate a null distribution of a performance metric and a corresponding test statistic. Testing conditional independence is especially useful in causal inference modeling.

### Key Features:
- Generates null distributions and test statistics using permutation testing.
- Computes p-values and provides visualization of distributions.
- Supports the machine learning algorithms: linear models (`lm`), random forests, and gradient boosting (`xgboost`).
- Allows for custom machine learning functions and performance metrics.

## Installation
First things first, you can the development version of `CCI` from GitHub:


```r
install.packages("devtools")
devtools::install_github("khliland/CCI")
library(CCI)
```

## 1. Basic Usage

### Simulating Data

First, define functions to simulate different types of data. In all the simulate data functions 'X' and 'Y' are independent only when conditioned on both 'Z1' and 'Z2', conditioning on either one alone is not sufficient. 

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
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

SinusoidalData <- function(N, a = 1){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  Z <- Z1 + Z2
  X = exp(-(Z)^2 / 2) * sin(a * (2*Z1 + 0.1*Z2)) + rnorm(N,0,0.1)
  Y = exp(-(Z)^2 / 2) * sin(a * (2*Z2 + 0.1*Z1)) + rnorm(N,0,0.1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
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

### Handling Different Data Types

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

### Testing CI in dagitty DAGs

As we have seen above, the absolute bare minimum arguments which need to be provided are `formula` and `data`, or `dag` and `data`. The formula must be of class formula and of the form Y ~ X + V + Z + ... etc or Y ~ X | V + Z + ... etc for testing the condition Y _||_ X | V, Z, ... .

The `dag` argument in `CCI.test()` can take a `dagitty` class object, representing a Directed Acyclic Graph (DAG). DAGs often contain multiple testable conditional independence statements. The specific statement you want to test is determined by the `dag_n` argument (with the default being `1`). Here’s a quick example:


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

### ⏩ Speeding Things Up with the `p` Argument
The `p` argument controls the proportion of data used for training the model, with the default set to `0.8`. If you’re dealing with a large dataset, you might want to set `p` to a lower value, like `0.1`, to speed up the process and increase precision. Here's how you can do it:

```r
set.seed(1983)
dat <- SinusoidalData(20000)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, method = 'xgboost', parametric = T, p = 0.1)
CCI.test(formula = Y ~ X | Z1, data = dat, method = 'xgboost', parametric = T, p = 0.1)
```
In this case, only 10% of the data is used for training, making the test faster (although still slow) and potentially more precise

### 🌀 The `nperm` Argument

The `nperm` argument controls the number of Monte Carlo samples used to generate the null distribution. Adjusting `nperm` can help speed up testing, but be aware that a lower number of samples might lead to less precise results. Here’s an example:

```r
set.seed(1983)
dat <- TrigData(1500)
dat$Y <- dat$Y - 1 

CCI.test(formula = Y ~ X | Z1, data = dat, method = 'xgboost', data_type = "categorical", num_class = 3, nperm = 100, parametric = TRUE)
```

In this example, we use only 100 permutations, which speeds up the test but might slightly reduce precision.

### ⏩ Combining `p` and `nperm` for Faster, Precise Testing

You can also combine the `p` and `nperm` arguments for both speed and precision. For instance, by using only one-third of the data for training (`p = 1/3`) and increasing the number of permutations to 300, you can create a robust null distribution while keeping the test efficient:

```r
set.seed(1983)
dat <- TrigData(1500)
dat$Y <- dat$Y - 1 
p = 1/3
CCI.test(formula = Y ~ X | Z1, data = dat, method = 'xgboost', data_type = "categorical", num_class = 3, p = p, nperm = 300, parametric = TRUE)
```
### 🛠️ Passing Other Arguments to the Machine Learning Function
When using different machine learning methods with `CCI.test()`, you can fine-tune the models by passing in additional arguments specific to the algorithm. For example, the `nrounds` argument controls the number of trees in `rf` and `xgboost` (default is 120). You can also adjust parameters like `max.depth`, `min.node.size`, and `sample.fraction` to control the depth of the trees, minimum node size, and fraction of the data used, respectively.

In the example below, we set the number of trees to 100, limit the maximum depth of the trees to 6 (unlimited by default), reduce the minimum node size to 4 (default is 5), and use 70% of the data (default is 100%). These adjustments help speed up the estimation process:

```r
set.seed(1979)
dat <- NonLinNormal(1000)
CCI.test(formula = Y ~ X | Z2, data = dat, parametric = TRUE, nrounds = 100, max.depth = 6, min.node.size = 4, sample.fraction = 0.7)
```

By reducing the number of trees, limiting tree depth, and using a smaller sample fraction, you can significantly speed up the testing process, however, the settings on the machine learning function should ideally be determined by pre tuning. 

The CCI package leverages the `xgboost` package to estimate the null distribution via the `xgb.train()` function. You can pass in various arguments to fine-tune the performance of `xgboost`. For example:

```r
set.seed(69)
dat <- NonLinNormal(1000)
CCI.test(formula = Y ~ X | Z2, data = dat, method = "xgboost", parametric = TRUE, nthread = 5, subsample = 0.7, lambda = 0.8, objective = "reg:pseudohubererror", seed = 2)
```
In this example:

- **`nthread = 5`**: Uses 5 threads for parallel processing, hopefully speeding up computation.
- **`subsample = 0.7`**: Uses 70% of the data for each boosting round, helping prevent overfitting and speed up computation.
- **`lambda = 0.8`**: Adds L2 regularization to the weights, making the model more robust.
- **`objective = "reg:pseudohubererror"`**: Changes the loss function to pseudohuber, which is more resistant to outliers than squared error (the default).

### Custom performance metric through the metricfunc argument. 
The default performance metrics used by CCI.test() are RMSE (Root Mean Square Error) for continuous outcomes, and Kappa scores for binary and categorical outcomes. However, a user can define his or her own custom performance metric. The metrifunc argument must take a function with the inputs `data`, `model`, `test_indices` and `test_matrix`, and the output must a numeric value. The argument `test_matrix` is only intended for cases where `xgboost` is used as method. Here is an example of how a function calculating \(R^2\) can be implemented with the method = `xgboost`.

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
When one is using a custom performance metric, one must also define the argument `tail`. If the a higher value of the performance metric indicate a better model in terms of prediction use tail = "right" and "left" otherwise. 



### The CCI metodology
Here's how the CCI test works testing the statement\( X \perp\!\!\!\perp Y \mid Z \):

1. Start by permuting \(Y\) into \(Y^p\) 
2. Then take p size (where \(0 <p < 1\) subset of the data and estimate the relationship \(X = f(Y^p, Z)\) using the p subset of data. 
3. Use the remaining \(1-p\) to calculate the performance of the predictions (e.g., RMSE, accuracy) from \(X = f(Y^p, Z)\).
4. In such a way we generate the null distribution by doing the above steps, say 500 times.
5. The test statistic is then generated by the same process only replacing the relationship \(X = f(Y^p, Z)\) with \(X = f(Y, Z)\).

The observed test statistic is then compared against the null distribution generated. If the observed test statistic falls far from the bulk of the null distribution, it suggests that the relationship between \(X\) and \(Y\) given \(Z\) is stronger than what would be expected under the null hypothesis of conditional independence.









