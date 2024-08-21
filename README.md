# CCI
The CCI (Computational Conditional Independence) package is an R package designed to perform conditional independence tests using machine learning methods combined with Monte Carlo cross validation. It enables users to test whether two variables are conditionally independent given a set of conditioning variables. The package supports a range of machine learning algorithms, including linear models (lm), random forests, and gradient boosting (xgboost). 

Key features include the ability to generate test statistics and null distributions through permutation testing, compute p-values, and visualize the results. The package is flexible, allowing users to customize their analysis with custom machine learning functions and performance metrics. It is particularly useful in causal inference and structural equation modeling, where understanding conditional independence is crucial.

 
## Installation

You can install the development version of `CCI` from GitHub with:

install.packages("devtools") 
devtools::install_github("username/PackageName")

### 4. Basic Usage

## Example

Here's a basic example of how to use the `CCI` package:
