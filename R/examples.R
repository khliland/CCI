# Examples for article
set.seed(1985)
dat <- NormalData(400)
CCI.test(formula = Y ~ X | Z1 + Z2, data = dat)

set.seed(1)
CCI.test(formula = Y ~ X | Z1,
         data = dat)
set.seed(1)
CCI.test(formula = Y ~ X | Z1,
         data = dat, parametric = T)
