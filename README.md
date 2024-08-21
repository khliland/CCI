# CCI
The CCI (Computational Conditional Independence) package is an R package designed to perform conditional independence tests using machine learning methods combined with Monte Carlo cross validation. It enables users to test whether two variables are conditionally independent given a set of conditioning variables. The package supports a range of machine learning algorithms, including linear models (lm), random forests, and gradient boosting (xgboost). 

Key features include the ability to generate test statistics and null distributions through permutation testing, compute p-values, and visualize the results. The package is flexible, allowing users to customize their analysis with custom machine learning functions and performance metrics. It is particularly useful in causal inference and structural equation modeling, where understanding conditional independence is crucial.

 
## Installation

You can install the development version of `CCI` from GitHub with:

install.packages("devtools") 
devtools::install_github("https://github.com/khliland/CCI")
library(CCI)


### 4. Basic Usage

## Example
First we define a simple data generating function where y and x are functions of z1 and z2 and random noise. 

gen_data <- function(N){
  z1 <- rnorm(N,0,1)
  z2 <- rnorm(N,0,1)
  x <- rnorm(N, z1 + z2 + z1*z2, 1)
  y <- rnorm(N, z1 + z2 + z1*z2, 1)
  df <- data.frame(z1, z2, x, y)
  return(df)
}
Here's a basic example of how to use the `CCI` package.
