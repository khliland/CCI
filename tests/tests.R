# Testing script.
# This script is for running code bits and chunks, feel free to overwrite as you see fit. 
# Example usage of Nullfunc with  parameters
library(dplyr)
# Continuous outcome
df <- data.frame(x1 = rnorm(200), x2 = rnorm(200), x3 = rnorm(200), x4 = rnorm(200), y = rnorm(200))
result_xgb <- null.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "xgboost", data_type = "continuous", nperm = 200)
result_xgb
result_rf <- null.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "rf",  nperm = 200, nrounds = 200)
result_rf
result_lm <- null.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "lm", data_type = "continuous", nperm = 200, lm_family = gaussian(link = "identity"))
result_lm
hist(result_rf$distribution)
hist(result_xgb$distribution, col = "blue")
hist(result_lm$distribution, col = "red")
n = 200
df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n), y = rbinom(n, 1, 0.5))
result_xgb <- null.gen(Y = "y", 
                       X = "x1", 
                       Z = c("x2", "x3", "x4"), 
                       data = df, 
                       method = "xgboost", 
                       data_type = "binary", 
                       nperm = 200, 
                       eta = 0.1, 
                       max_depth = 3)
result_xgb
result_rf <- null.gen(Y = "y", 
                      X = "x1", 
                      Z = c("x2", "x3", "x4"), 
                      data = df, 
                      method = "rf", 
                      data_type = "binary", 
                      nperm = 200, 
                      nrounds = 500, 
                      probability = TRUE)
result_rf
result_lm <- null.gen(Y = "y", 
                      X = "x1", 
                      Z = c("x2", "x3", "x4"), 
                      data = df, 
                      data_type = "binary",
                      method = "lm", 
                      nperm = 200, 
                      lm_family = binomial(link = "logit"))
hist(result_rf$distribution)
hist(result_xgb$distribution, col = "blue")
hist(result_lm$distribution)















