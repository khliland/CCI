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
data <- normal_data(1400)
result_rf <- perm.test(formula = "Y ~ X | Z1, Z2", data = data, method = "rf",p = 0.8,  nperm = 2000, nrounds = 99, parametric = TRUE)
result_rf
summary_result <- summary(result_rf)
print(summary_result)
plot_null_distribution(result_rf)

plot <- QQplot()

result <- perm.test(formula = "Y ~ X | Z1", data = data, method = "rf", nperm = 1000, parametric = TRUE)
summary_result <- summary(result)
print(summary_result)

result_xgb <- perm.test(formula = "Y ~ X | Z1, Z2", data = data, method = "xgboost", nperm = 1000, parametric = TRUE)
summary_result <- summary(result_xgb)
print(summary_result)
plot_null_distribution(result_xgb)


result_xgb <- perm.test(formula = "Y ~ X | Z1", data = data, method = "xgboost", nperm = 1000, parametric = TRUE)
summary_result <- summary(result_xgb)
print(summary_result)

# Using a linear model
result <- perm.test(formula = "Y ~ X | Z1, Z2", data = data, method = "lm", nperm = 2000, parametric = TRUE, lm_family = gaussian(link = "identity"))
summary_result <- summary(result)
print(summary_result)
plot_null_distribution(result)

result <- perm.test(formula = "Y ~ X | Z1", data = data, method = "lm", nperm = 2000, parametric = TRUE, lm_family = gaussian(link = "identity"))
summary_result <- summary(result)
print(summary_result)

set.seed(2)
data <- non_lin_normal(5000)
result <- perm.test(formula = "Y ~ X | Z1, Z2", data = data, method = "rf", nperm = 1000, parametric = TRUE)
summary_result <- summary(result)
print(summary_result)
plot_null_distribution(result)

data <- non_lin_normal(5000)
result <- perm.test(formula = "Y ~ X | Z1", data = data, method = "rf", nperm = 1000, parametric = TRUE)
summary_result <- summary(result)
print(summary_result)
plot_null_distribution(result)

dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
cci <- perm.test("y ~ x1 | x2", data = dat)
plot <- QQplot(cci, 1000)
print(plot)
 
# Binary 


# Categorical


# QQPlot
dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
cci <- perm.test("y ~ x1 | x2", data = dat)
QQplot(cci)


