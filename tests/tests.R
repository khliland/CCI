# Testing script.
# This script is for running code bits and chunks, feel free to overwrite as you see fit. 
# Example usage of Nullfunc with  parameters
library(dplyr)
# Continuous outcome
df <- data.frame(x1 = rnorm(500), x2 = rnorm(500), x3 = rnorm(500), x4 = rnorm(500), y = rnorm(500))
null_xgb <- test.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "xgboost", data_type = "continuous", nperm = 500, eta = 0.1, max_depth = 3,  permutation = TRUE)
test_xgb <- test.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "xgboost", data_type = "continuous", nperm = 1, eta = 0.1, max_depth = 3,  permutation = FALSE)
test_xgb <- test.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "xgboost", data_type = "continuous", nperm = 100, eta = 0.1, max_depth = 3,  permutation = FALSE)

null_rf <- test.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "rf", data_type = "continuous", nperm = 200, nrounds = 500)
null_lm <- test.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "lm", data_type = "continuous", nperm = 500, lm_family = gaussian(link = "identity"))

hist(null_xgb$distribution , ylim = range(0,100), xlim = range(0.5, 1.5), breaks = 25)
hist(null_rf$distribution, col = rgb(173/255, 216/255, 230/255, alpha = 0.5), add = T, breaks = 25)
hist(null_lm$distribution, col = rgb(200/255, 16/255, 230/255, alpha = 0.5), add = T, breaks = 20)
n = 500
df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n), y = rbinom(n, 1, 0.5))
result_xgb <- test.gen(Y = "y", 
                       X = "x1", 
                       Z = c("x2", "x3", "x4"), 
                       data = df, 
                       method = "xgboost", 
                       data_type = "binary", 
                       nperm = 500, 
                       eta = 0.1, 
                       max_depth = 3,
                       permutation = TRUE)
result_rf <- test.gen(Y = "y", 
                      X = "x1", 
                      Z = c("x2", "x3", "x4"), 
                      data = df, 
                      method = "rf", 
                      data_type = "binary", 
                      nperm = 500, 
                      nrounds = 500, 
                      probability = TRUE)
result_lm <- test.gen(Y = "y", 
                      X = "x1", 
                      Z = c("x2", "x3", "x4"), 
                      data = df, 
                      data_type = "binary",
                      method = "lm", 
                      nperm = 500, 
                      lm_family = binomial(link = "logit"))
hist(result_xgb$distribution)
hist(result_rf$distribution, col = rgb(173/255, 216/255, 230/255, alpha = 0.5), add = T)
hist(result_lm$distribution, col = rgb(100/255, 2/255, 230/255, alpha = 0.5), add = T)















