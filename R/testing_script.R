# Testing script.
# This script is for running code bits and chunks, feel free to overwrite as you see fit. 
# Example usage of Nullfunc with  parameters
set.seed(123)
df <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
result_xgb <- Nullfunc(Y = "y", X = "x1", Z = c("x2"), data = df, method = "xgboost", nperm = 500)
hist(result_xgb)
result_lm <- Nullfunc(Y = "y", X = "x1", Z = c("x2"), data = df, method = "lm", nperm = 500, lm_family = gaussian(link = "identity"))
hist(result_lm, add = T)
