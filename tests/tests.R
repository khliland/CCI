# Testing script.
# This script is for running code bits and chunks, feel free to overwrite as you see fit. 
# Example usage of Nullfunc with  parameters
library(dplyr)
# Continuous 
normal_data <- function(N){
  Z1 <- rnorm(N,0,1)
  Z2 <- rnorm(N,0,1)
  X <- rnorm(N, Z1 + Z2, 1)
  Y <- rnorm(N, Z1 + Z2, 1)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}
non_lin_normal <- function(N){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = exp(Z1*Z2) + rnorm(N,0,1)
  Y <- Z1*Z2 + rnorm(N,0,1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

set.seed(1)
data <- normal_data(1000)
result_rf <- perm.test(formula = "Y ~ X | Z1, Z2", data = data, method = "rf",p = 0.8,  nperm = 1000, nrounds = 99, parametric = TRUE)
result_rf
summary_result <- summary(result_rf)
print(summary_result)

result <- perm.test(formula = "Y ~ X | Z1", data = data, method = "rf", nperm = 1000, parametric = TRUE)
summary_result <- summary(result)
print(summary_result)

result_xgb <- perm.test(formula = "Y ~ X | Z1, Z2", data = data, method = "xgboost", nperm = 1000, parametric = TRUE)
summary_result <- summary(result_xgb)
print(summary_result)

result_xgb <- perm.test(formula = "Y ~ X | Z1", data = data, method = "xgboost", nperm = 1000, parametric = TRUE)
summary_result <- summary(result_xgb)
print(summary_result)

# Using a linear model
result <- perm.test(formula = "Y ~ X | Z1, Z2", data = data, method = "lm", nperm = 2000, parametric = TRUE, lm_family = gaussian(link = "identity"))
summary_result <- summary(result)
print(summary_result)

result <- perm.test(formula = "Y ~ X | Z1", data = data, method = "lm", nperm = 2000, parametric = TRUE, lm_family = gaussian(link = "identity"))
summary_result <- summary(result)
print(summary_result)

set.seed(2)
data <- non_lin_normal(1000)
result <- perm.test(formula = "Y ~ X | Z1, Z2", data = data, method = "rf", nperm = 1000, parametric = TRUE)
summary_result <- summary(result)
print(summary_result)

result <- perm.test(formula = "Y ~ X | Z1", data = data, method = "rf", nperm = 1000, parametric = TRUE)
summary_result <- summary(result)
print(summary_result)


# Binary 


# Categorical





