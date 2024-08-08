# Test script for the CCI package
devtools::load_all()
library(CCI)
set.seed(123)
dat <- data.frame(
  x1 = rnorm(800),
  x2 = rnorm(800),
  x3 = rnorm(800),
  x4 = rnorm(800),
  y = rnorm(800)
)
CCI.test(formula = "y ~ x1 | x2, x3, x4", data = dat, nperm = 1000)
