# CCI Package

The CCI (Computational Conditional Independence) package in R is designed to test for conditional independence between two variables given a set of conditioning variables. It utilizes machine learning models, permutation testing, and Monte Carlo cross-validation to estimate a null distribution of a performance metric and a corresponding test statistic. This method is especially useful in causal inference modeling.

### Key Features:
- Generates null distributions and test statistics using permutation testing.
- Computes p-values and provides visualization of distributions.
- Supports the machine learning algorithms: linear models (`lm`), random forests, and gradient boosting (`xgboost`).
- Allows for custom machine learning functions and performance metrics.

## Installation

Install the development version of `CCI` from GitHub:

```r
install.packages("devtools")
devtools::install_github("khliland/CCI")
library(CCI)
```

## 1. Basic Usage

### Simulating Data

First, define functions to generate different types of data:

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
 
```

### Testing Conditional Independence

The generated data structure assumes that 'X' and 'Y' are independent when conditioned on both 'Z1' and 'Z2', but not when conditioned on either one alone. Below is an example of how to test this hypothesis using the CCI package.

```r
set.seed(1985)
dat <- NormalData(400)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat)
CCI.test(formula = Y ~ X | Z1, data = dat, parametric = TRUE)
```

These tests will plot the null distribution along with the calculated test statistic. When the `parametric` argument is set to `TRUE`, the method assumes that the null distribution is approximately Gaussian.

### Handling Different Data Types

Depending on the data type of `Y` in the formula `Y ~ X | Z1 + Z2`, you can adjust the `data_type` parameter to `"continuous"` (default), `"binary"`, or `"categorical"`. Here is an example with binary data:

```r
set.seed(1985)
dat <- BinaryData(500)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = "binary")
CCI.test(formula = Y ~ X | Z2, data = dat, data_type = "binary", method = "xgboost")
```

In the second example, we use `xgboost` as the machine learning algorithm. The CCI package offers three built-in methods: `lm` (linear model), `rf` (random forest, the default), and `xgboost` (extreme gradient boosting). While random forest is the default due to its balance of speed and accuracy, `xgboost` is more robust and recommended especially for binary and categorical data types. Custom machine learning algorithms can also be defined by the user. 

Testing conditional independence with categorical data types is difficult. The CCI-package can handle such cases,  but requires quit large data sets, again, we recommend to use xgboost. Unfortunately, it is also a little slow.

```r
set.seed(2097)
dat <- TrigData(1000)
dat$Y <- dat$Y - 1 # Xgboost requires that a categorical outcome 'starts' on 0
CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = 'categorical', method = "xgboost", num_class = 3)
# Note that we need to set the num_class argument for the xgb.train()
CCI.test(formula = Y ~ X | Z1, data = dat, data_type = 'categorical', method = "xgboost", num_class = 3)
```

rf is fast, however, you can switch to a linear parametric model which is even faster, though potentially less precise. If we can assume that the regression 'Y ~ X + Z1 + Z2' is well estimated by a parametric model, 'lm' is a good choice. When using the 'lm' method, you must also define the family argument as required by glm(). Since 'lm' is much faster, we generate 2000 Monte Carlo samples for the null distribution. Here’s how to do it:

```r
set.seed(1984)
dat <- NormalData(200)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T)
CCI.test(formula = Y ~ X | Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T)

```
In the last case we fail to reject to null although it should be rejected. It might be the case that we have insufficient power. An extra level of analysis in CCI is to create QQplots over p-values, to see if they approximately follows a uniform distribution, here is how to do it. 

```r
set.seed(1984)
dat <- NormalData(200)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T)
CCI.test(formula = Y ~ X | Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T)

test <- CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T, plot = F)
QQplot(test)

test <- CCI.test(formula = Y ~ X | Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T, plot = F)
QQplot(test)
```


### 2. Arguments in CCI.test()

The absolute bare minimum arguments which need to be provided are `formula` and `data`, or `dag` and `data`. The formula must be of class formula and of the form Y ~ X + V + Z + ... etc or Y ~ X | V + Z + ... etc for testing the condition Y _||_ X | V, Z, ... .

The argument `dag` can take a `dagitty` class object of a directed acyclic graph (DAG). Most dagitty DAG's has several testable conditional independence statements, which one we want to test in our DAG is determined by the `dag_n` argument (default = 1). Here is an example of how to do it. 

```r
set.seed(1984)
dat <- NormalData(200)

```

The argument 'p' is the proportion of data used for training the model and default is 0.8. When handling large data sets it can be particularly useful to set the p argument relatively low. In the example below, we only use 10 % of the data during training, by setting p = 0.1 as shown below. This speeds up the testing, and makes in increase precision.
```r
set.seed(1983)
dat <- sinusoidal(20000)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, method = 'xgboost', parametric = T, p = 0.1)
CCI.test(formula = Y ~ X | Z1, data = dat, method = 'xgboost', parametric = T, p = 0.1)
```
### 3. QQ plots

### X. A note on formula usage









