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

Below is a basic example demonstrating how to test this hypothesis using the CCI package.

The data-generating structure in the examples below implies that 'x' and 'y' are independent when conditioned on both 'z1' and 'z2'. Conditioning on either 'Z1' or 'Z2' should result in a low p-value. The basic test also plots the null distribution with and the calculate test statistic. When the parametric argument is set to TRUE, one assumes that the null distribution is approximately Gaussian. 
```r
set.seed(1985)
dat <- normal_data(400)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat)
CCI.test(formula = Y ~ X | Z1, data = dat, parametric = T)
```


Depending on the type of 'y' variable on the left side of the condition bar in expression 'y ~ x | z1 + z2' one can account for this in testing by setting the type to either continous (default) binary or categorical, with the 'data_type' parameter, here are some examples with a binary 'y'.

```r
set.seed(1985)
dat <- binary_data(500)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = "binary")
CCI.test(formula = Y ~ X | Z1, data = dat, data_type = "binary")
CCI.test(formula = Y ~ X | Z2, data = dat, data_type = "binary")
CCI.test(formula = Y ~ X | Z2, data = dat, data_type = "binary", method = "xgboost")
CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = "binary", method = "lm", family = binomial(link = "logit"))
CCI.test(formula = Y ~ X | Z2, data = dat, data_type = "binary", method = "lm", family = binomial(link = "logit"))

```
Here are some examples with the categorical data type:
```r
set.seed(2020)
dat <- CategorizeInteractiondData(400)

CCI.test(formula = y ~ x | z1 + z2, data = dat, data_type = 'categorical', parametric = T)
```

### 2. Extended Usage

By default, the testing method used is random forest (rf), which provides a balance between speed and accuracy. However, you can switch to a linear parametric model for faster, though potentially less precise, results. Note that when using the 'lm' method, you must also define the family parameter as required by glm(). Here’s how to do it:

```r
set.seed(1984)
dat <- normal_data(400)

CCI.test(formula = Y ~ X | Z1 + Z2,
         data = dat,
         method = 'lm',
         family = gaussian(),
         parametric = T)
```
If you prefer a more robust approach, you can use the xgboost method. This method is particularly useful in complex cases and larger datasets, however, xgboost is much slower. Here’s how you can apply it:

```r
set.seed(1983)
dat <- normal_data(400)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, method = 'xgboost', parametric = T)
```
You can customize the behavior of the xgboost algorithm by passing additional arguments, such as the eta and nrounds parameters. When handling large data sets it can be particularly useful to set the p argument reltaivly low. For example, we can only use 10 % of the data during training, by setting p = 0.1 as shown below:
```r
set.seed(1983)
dat <- sinusoidal(20000)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, method = 'xgboost', parametric = T, p = 0.1)
```
### 3. QQ plots

### X. A note on formula usage









