# Testing script.
# This script is for running code bits and chunks, feel free to overwrite as you see fit. 
# Example usage of Nullfunc with  parameters

df <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), x4 = rnorm(100), y = rnorm(100))
result_xgb <- null.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "xgboost", nperm = 100)
result_xgb
hist(result_xgb$distribution)
result_rf <- null.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "RandomForest", nperm = 100)
result_rf
hist(result_rf$distribution)

result_lm <- null.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = df, method = "lm", nperm = 500, lm_family = gaussian(link = "identity"))
hist(result_lm$distribution, col = "blue")

df <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))

formula <- "y ~ x1 | x2"

result <- perm.test(formula = formula, data = df, method = "xgboost", nperm = 50, nrounds = 50, eta = 0.1, max_depth = 3, nthread = 1)
print(result)
