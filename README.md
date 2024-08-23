# CCI
The CCI (Computational Conditional Independence) package is an R package designed to perform computational conditional independence. The testing applies machine learning methods combined with Monte Carlo cross validation. It enables users to test whether two variables are conditionally independent given a set of conditioning variables. The package supports a range of machine learning algorithms, including linear models (lm), random forests, and gradient boosting (xgboost). 

Key features include the ability to generate test statistics and null distributions through permutation testing, compute p-values, and visualize the results. The package is flexible, allowing users to customize their analysis with custom machine learning functions and performance metrics. Testing conditional independence is particularly useful in causal inference modelling. 

## Installation

You can install the development version of `CCI` from GitHub with:

```r

install.packages("devtools") 
devtools::install_github("https://github.com/khliland/CCI")
library(CCI)
```

### 1. Basic Usage

First we define functions to simulate some data.
```r
NormalData <- function(N){
  Z1 <- rnorm(N,0,1)
  Z2 <- rnorm(N,0,1)
  X <- rnorm(N, Z1 + Z2, 1)
  Y <- rnorm(N, Z1 + Z2, 1)
  return(data.frame(Z1, Z2, X, Y))
}
BinaryData <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)
  X <- ifelse(rnorm(N, Z1 + Z2 + Z1*Z2, 1) < 0, 1, 0)
  Y <- ifelse(rnorm(N, Z1 + Z2 + Z1*Z2, 1) < 0, 1, 0)
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
```

The data-generating structure of all the examples in this readme is that 'X' and 'Y' are independent when conditioned on both 'Z1' and 'Z2'. 'X' and 'Y' are not independent when conditioning on either 'Z1' or 'Z2'.   Below is a basic example demonstrating how to test this hypothesis using the CCI package.

```r
set.seed(1985)
dat <- NormalData(400)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat)
CCI.test(formula = Y ~ X | Z1, data = dat, parametric = T)
```
The basic test also plots the null distribution with and the calculate test statistic. When the parametric argument is set to TRUE, one assumes that the null distribution is approximately Gaussian.

Depending on the data type of the Y variable on the left side of the condition bar in the expression Y ~ X | Z1 + Z2, you can change the data_type parameter to either "continuous" (default), "binary", or "categorical". Below is an example when Y (and X) is binary:

```r

set.seed(1985)
dat <- BinaryData(500)

set.seed(1985)
dat <- BinaryData(500)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = "binary")
CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = "binary", method = "xgboost")
```
In the second example, we set method = "xgboost", which applies extreme gradient boosting as the machine learning algorithm for testing. The CCI package provides three built-in methods: the linear model "lm", random forest "rf" (default), and extreme gradient boosting ("xgboost"). Random forest is the default because it provides a balance between speed and accuracy. However, "xgboost" is more robust, and is recommended. You can also define your own custom machine learning algorithm. 

Testing conditional independence with categorical data types is difficult. The CCI-package can handle such cases,  but requires quit large data sets, again, we recommend to use xgboost. Unfortunately, it is also a little slow.

```r
set.seed(2097)
dat <- TrigData(1000)
dat$Y <- dat$Y - 1 # Xgboost requires that a categorical outcome 'starts' on 0
CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = 'categorical', method = "xgboost", num_class = 3)
# Note that we need to set the num_class argument for the xgb.train()
CCI.test(formula = Y ~ X | Z1, data = dat, data_type = 'categorical', method = "xgboost", num_class = 3)
```

By default, the testing method used is random forest (rf), which  is fast. However, you can switch to a linear parametric model which is even faster, though potentially less precise. However, if we can assume that the regression 'Y ~ X + Z1 + Z2' is well estimated by a parametric model, 'lm' is a good choice. When using the 'lm' method, you must also define the family argument as required by glm(). Since 'lm' is much faster, we generate 2000 Monte Carlo samples for the null distribution. Here’s how to do it:

```r
set.seed(1984)
dat <- NormalData(200)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T)
CCI.test(formula = Y ~ X | Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T)

```
In the last case we fail to reject to null although it should be rejected. It might be the case that we have insufficient power. An extra level of analysis in CCI is to creat QQplots over p-values, to see if they approximately follows a uniform distribution, here is how to do it. 

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

The absolute bare minimum arguments which need to be provided are 'formula' and 'data', or 'dag' and 'data'. The formula must be of class formula and of the form 'Y ~ X + V + Z + ... etc' or 'Y ~ X | V + Z + ... etc' for testing the condition 'Y _||_ X | V, Z, ...'. 
plot

The argument 'p' is the proportion of data used for training the model and default is 0.8. When handling large data sets it can be particularly useful to set the p argument relatively low. In the example below, we only use 10 % of the data during training, by setting p = 0.1 as shown below. This speeds up the testing, and makes in increase precision.
```r
set.seed(1983)
dat <- sinusoidal(20000)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, method = 'xgboost', parametric = T, p = 0.1)
CCI.test(formula = Y ~ X | Z1, data = dat, method = 'xgboost', parametric = T, p = 0.1)
```
### 3. QQ plots

### X. A note on formula usage









