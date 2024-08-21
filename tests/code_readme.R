# install.packages("devtools") # Uncomment if necessary
devtools::install_github("https://github.com/khliland/CCI")
library(CCI)

gen_data <- function(N){
  z1 <- rnorm(N,0,1)
  z2 <- rnorm(N,0,1)
  x <- rnorm(N, z1 + z2, 1)
  y <- rnorm(N, z1 + z2, 1)
  df <- data.frame(z1, z2, x, y)
  return(df)
}
set.seed(123)
dat <- gen_data(400)

CCI.test(formula = y ~ x | z1 + z2, data = dat, seed = 1880)
CCI.test(formula = y ~ x | z1, data = dat, seed = 1660)
