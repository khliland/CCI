# Test script for the CCI package
devtools::load_all()
library(CCI)
set.seed(123)
dat <- data.frame(
  x1 = rnorm(200),
  x2 = rnorm(200),
  x3 = rnorm(200),
  x4 = rnorm(200),
  y = rnorm(200)
)
test <- perm.test(formula = "y ~ x1 | x2, x3, x4", data = dat)
summary(test)
plot(test, title = "Test")

CCI.test(formula = "y ~ x1 | x2, x3, x4", data = dat, nperm = 100, method = "lm")

dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
cci <- perm.test("y ~ x1 | x2", data = dat)
QQplot(cci)
dag <- dagitty('dag {
  X -> Y
  Y -> Z

}')
plot(dag)
t <- impliedConditionalIndependencies(dag)


result <- perm.test(formula = "Y ~ X | Z1", data = data, method = "rf", nperm = 1000, parametric = TRUE)
summary_result <- summary(result)
print(summary_result)
