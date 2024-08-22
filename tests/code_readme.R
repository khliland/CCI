install.packages("devtools") # Uncomment if necessary
devtools::install_github("https://github.com/khliland/CCI", force =TRUE)
library(CCI)
#-------------------------------------------------------------------------------
set.seed(1985)
dat <- normal_data(400)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat)
CCI.test(formula = Y ~ X | Z1, data = dat, parametric = T)

#-------------------------------------------------------------------------------
set.seed(1985)
dat <- binary_data(500)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = "binary")
CCI.test(formula = Y ~ X | Z1, data = dat, data_type = "binary")
CCI.test(formula = Y ~ X | Z2, data = dat, data_type = "binary")
CCI.test(formula = Y ~ X | Z2, data = dat, data_type = "binary", method = "xgboost")
CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = "binary", method = "lm", family = binomial(link = "logit"))
CCI.test(formula = Y ~ X | Z2, data = dat, data_type = "binary", method = "lm", family = binomial(link = "logit"))

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
set.seed(2020)
dat <- simulateExpLogData(500)

CCI.test(formula = Y ~ X | Z1, data = dat, data_type = 'categorical')
#-------------------------------------------------------------------------------

set.seed(1984)
dat <- normal_data(400)

CCI.test(formula = Y ~ X | Z1 + Z2,
         data = dat,
         method = 'lm',
         family = gaussian(),
         parametric = T)
#-------------------------------------------------------------------------------

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, method = 'xgboost', parametric = T)
#-------------------------------------------------------------------------------
set.seed(1983)
dat <- sinusoidal(20000)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, method = 'xgboost', parametric = T, p = 0.1)
#-------------------------------------------------------------------------------
citation()
