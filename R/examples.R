# Examples for article
set.seed(1)
data <- data.frame(x1 = rnorm(200),
x2 = rnorm(200),
x3 = rnorm(200),
x4 = rnorm(200),
y = rnorm(200))
null_dist <- test.gen(formula = y ~ x1 | x2 + x3 + x4, data = data, permutation = TRUE, nperm = 100, method = "rf")
test_stat <- test.gen(formula = y ~ x1 | x2 + x3 + x4, data = data, permutation = FALSE, nperm = 1, method = "rf")
get_pvalues(dist = null_dist$distribution, test_statistic = test_stat$distribution, tail = 'right')

