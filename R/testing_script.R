# Testing script.
# This script is for running code bits and chunks, feel free to overwrite as you see fit. 
# Example usage of Nullfunc with  parameters

df <- data.frame(x1 = rnorm(500), x2 = rnorm(500), x3 = rnorm(500), x4 = rnorm(500), y = rnorm(500))
result_xgb <- null.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "xgboost", nperm = 100)
result_xgb
hist(result_xgb$distribution)
result <- null.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "gbm", nperm = 100)
result
hist(result_xgb$distribution)

result_lm <- Nullfunc(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "lm", nperm = 500, lm_family = gaussian(link = "identity"))
# hist(result_lm, add = T, col = "blue")


df <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))

formula <- "y ~ x1 | x2"

result <- perm.test(formula = formula, data = df, method = "xgboost", nperm = 50, nrounds = 50, eta = 0.1, max_depth = 3, nthread = 1)
print(result)
