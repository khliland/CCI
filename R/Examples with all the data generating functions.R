set.seed(2)
data <- NormalData(1000)
summary(CCI.test(formula = Y ~ X + Z1 + Z2, data = data))
summary(CCI.test(formula = Y ~ X + Z1, data = data))

data <- BinaryData(1000)
summary(CCI.test(formula = Y ~ X + Z1 + Z2, data = data, metric = "Kappa"))
summary(CCI.test(formula = Y ~ X + Z1, data = data, metric = "Kappa"))

data <- BivMultinominal(1000)
summary(CCI.test(formula = Y ~ X + Z1 + Z2, data = data, metric = "Kappa"))
summary(CCI.test(formula = Y ~ X + Z1, data = data, metric = "Kappa"))
summary(CCI.test(formula = Y ~ X + Z1, data = data, metric = "Kappa", method = "xgboost"))


