# CCI
The CCI (Computational Conditional Independence) package is an R package designed to perform conditional independence tests using machine learning methods combined with Monte Carlo cross validation. It enables users to test whether two variables are conditionally independent given a set of conditioning variables. The package supports a range of machine learning algorithms, including linear models (lm), random forests, and gradient boosting (xgboost). 

Key features include the ability to generate test statistics and null distributions through permutation testing, compute p-values, and visualize the results. The package is flexible, allowing users to customize their analysis with custom machine learning functions and performance metrics. It is particularly useful in causal inference and structural equation modeling, where understanding conditional independence is crucial.

 
## Installation

You can install the development version of `CCI` from GitHub with:

```r

install.packages("devtools") 
devtools::install_github("https://github.com/khliland/CCI")
library(CCI)
```

### 4. Basic Usage

## Example
First we define a simple data generating function where y and x are functions of z1 and z2 and random noise.  
```r
gen_data <- function(N){
  z1 <- rnorm(N,0,1)
  z2 <- rnorm(N,0,1)
  x <- rnorm(N, z1 + z2 + z1*z2, 1)
  y <- rnorm(N, z1 + z2 + z1*z2, 1)
  df <- data.frame(z1, z2, x, y)
  return(df)
}
```
The data generating structure in `gen_data` implies that 'x' and 'y' are independent conditioned on 'z1' and 'z2'. Here's a basic example of how one can test this with the `CCI` package.

```r
set.seed(123)
dat <- gen_data(400)

CCI.test(formula = y ~ x | z1 + z2, data = dat, seed = 1880)
CCI.test(formula = y ~ x | z1, data = dat, seed = 1660, parametric = T)
```
In the second example, we have set the parametric argument equal to TRUE. The default method for testing is random forest, which is fast. However we can change the method with the argument method, like so: 

```r
set.seed(1984)
dat <- gen_data(400)

CCI.test(formula = y ~ x | z1 + z2, data = dat, method = 'lm', family = gaussian(), seed = 321, parametric = T)
```

If we want to use the more robust but slower xgboost:

```r
dat <- gen_data(400)

CCI.test(formula = y ~ x | z1 + z2, data = dat, method = 'xgboost', seed = 321, parametric = T, seed = 1983)
```








