# install.packages("devtools") # Uncomment if necessary
devtools::install_github("https://github.com/khliland/CCI", force =TRUE)
library(CCI)
#-------------------------------------------------------------------------------
NormalData <- function(N){
  Z1 <- rnorm(N,0,1)
  Z2 <- rnorm(N,0,1)
  X <- rnorm(N, Z1 + Z2, 1)
  Y <- rnorm(N, Z1 + Z2, 1)
  return(data.frame(Z1, Z2, X, Y))
}
BinaryData <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)
  X <- ifelse(rnorm(N, Z1 + Z2 + Z1*Z2, 1) < 0, 1, 0)
  Y <- ifelse(rnorm(N, Z1 + Z2 + Z1*Z2, 1) < 0, 1, 0)
  return(data.frame(Z1, Z2, X, Y))
}
TrigData <- function(N) {
  Z1 <- runif(N, -pi, pi)
  Z2 <- rnorm(N)
  X <- numeric(N)
  Y <- numeric(N)

  for (i in 1:N) {
    X[i] <- ifelse(sin(Z1[i]) + cos(Z2[i]) > 1, 3,
                   ifelse(sin(Z1[i]) + cos(Z2[i]) > 0, 2,
                          ifelse(sin(Z1[i]) > -1, 1, 0)))

    Y[i] <- ifelse(cos(Z1[i]) - sin(Z2[i]) > 1, 3,
                   ifelse(cos(Z1[i]) - sin(Z2[i]) > 0, 2,
                          ifelse(cos(Z1[i]) > -1, 1, 0)))
  }
  return(data.frame(Z1, Z2, X, Y))
}

#-------------------------------------------------------------------------------
set.seed(1985)
dat <- NormalData(400)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat)
CCI.test(formula = Y ~ X | Z1, data = dat, parametric = T)
#-------------------------------------------------------------------------------
set.seed(1985)
dat <- BinaryData(500)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = "binary")
CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = "binary", method = "xgboost")
#-------------------------------------------------------------------------------
set.seed(2097)
dat <- TrigData(1000)
dat$Y <- dat$Y - 1 # Xgboost requires that a categorical outcome 'starts' on 0

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, data_type = 'categorical', method = "xgboost", num_class = 3)
# Note that we need to set the num_class argument for the xgb.train()
CCI.test(formula = Y ~ X | Z1, data = dat, data_type = 'categorical', method = "xgboost", num_class = 3)
#-------------------------------------------------------------------------------
set.seed(1984)
dat <- NormalData(200)

CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T)
CCI.test(formula = Y ~ X | Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T)
#-------------------------------------------------------------------------------
test <- CCI.test(formula = Y ~ X | Z1 + Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T, plot = F)
QQplot(test)

test <- CCI.test(formula = Y ~ X | Z2, data = dat, nperm = 2000, method = 'lm', family = gaussian(), parametric = T, plot = F)
QQplot(test)

#-------------------------------------------------------------------------------


library(dagitty)

NonLinNormal <- function(N){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = exp(Z1*Z2) + rnorm(N,0,1)
  Y <- Z1*Z2 + rnorm(N,0,1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

data <- NonLinNormal(500)

dag <- dagitty('dag {
  X
  Y
  Z1
  Z2
  Z1 -> X
  Z1 -> Y
  Z2 -> X
  Z2 -> Y
}')

result <- CCI.test(formula = NULL,
                   p = 0.7,
                   data = data,
                   dag = dag,
                   dag_n = 1,
                   nperm = 100,
                   parametric = T
)



