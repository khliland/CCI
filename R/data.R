
InteractiondData <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)
  X <- numeric(N)
  Y <- numeric(N)

  for (i in 1:N) {
    if (Z1[i] < 0 && Z2[i] < 0) {
      X[i] <- 0
    } else if (Z1[i] < 0 && Z2[i] >= 0) {
      X[i] <- 1
    } else if (Z1[i] >= 0 && Z2[i] < 0) {
      X[i] <- 2
    } else {
      X[i] <- 3
    }

    if (Z1[i] + Z2[i] < -1) {
      Y[i] <- 0
    } else if (Z1[i] + Z2[i] < 0) {
      Y[i] <- 1
    } else if (Z1[i] + Z2[i] < 1) {
      Y[i] <- 2
    } else {
      Y[i] <- 3
    }
  }

  data_frame <- data.frame(Z1, Z2, X, Y)

  return(data_frame)
}

ExpLogData <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)
  X <- numeric(N)
  Y <- numeric(N)

  for (i in 1:N) {
    X[i] <- ifelse(exp(Z1[i]) + Z2[i] > 1.5, 3,
                   ifelse(exp(Z1[i]) + Z2[i] > 0.5, 2,
                          ifelse(exp(Z1[i]) > 0, 1, 0)))
    Y[i] <- ifelse(log(abs(Z1[i]) + 1) + Z2[i] > 0.5, 3,
                   ifelse(log(abs(Z1[i]) + 1) + Z2[i] > 0, 2,
                          ifelse(log(abs(Z1[i]) + 1) > -0.5, 1, 0)))
  }

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

PolyData <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)
  X <- numeric(N)
  Y <- numeric(N)

  for (i in 1:N) {
    X[i] <- ifelse(Z1[i]^2 + Z2[i]^2 > 2, 3,
                   ifelse(Z1[i]^2 + Z2[i] > 0.5, 2,
                          ifelse(Z1[i] + Z2[i]^2 > 0, 1, 0)))

    Y[i] <- ifelse(Z1[i]^3 + Z2[i] > 1, 3,
                   ifelse(Z1[i]^2 - Z2[i]^2 > 0, 2,
                          ifelse(Z1[i] - Z2[i]^3 > -1, 1, 0)))
  }

  return(data.frame(Z1, Z2, X, Y))
}

NonLinearData <- function(N) {
  Z1 <- runif(N, -1, 1)
  Z2 <- runif(N, -1, 1)
  X <- numeric(N)
  Y <- numeric(N)


  for (i in 1:N) {

    if (sin(Z1[i] * pi) + Z2[i] > 1) {
      X[i] <- 3
    } else if (sin(Z1[i] * pi) + Z2[i] > 0.5) {
      X[i] <- 2
    } else if (sin(Z1[i] * pi) + Z2[i] > 0) {
      X[i] <- 1
    } else {
      X[i] <- 0
    }


    if (cos(Z1[i] * pi) + Z2[i] > 1) {
      Y[i] <- 3
    } else if (cos(Z1[i] * pi) + Z2[i] > 0.5) {
      Y[i] <- 2
    } else if (cos(Z1[i] * pi) + Z2[i] > 0) {
      Y[i] <- 1
    } else {
      Y[i] <- 0
    }
  }

  return(data.frame(Z1, Z2, X, Y))
}

ComplexCategorization <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)
  X <- numeric(N)
  Y <- numeric(N)

  for (i in 1:N) {
    X[i] <- ifelse(Z1[i] > 0 && Z2[i] > 0, 3,
                   ifelse(Z1[i] > 0 && Z2[i] <= 0, 2,
                          ifelse(Z1[i] <= 0 && Z2[i] > 0, 1, 0)))
    Y[i] <- ifelse(Z1[i] + Z2[i] > 1, 3,
                   ifelse(Z1[i] + Z2[i] > 0, 2,
                          ifelse(Z1[i] + Z2[i] > -1, 1, 0)))
  }

  return(data.frame(Z1, Z2, X, Y))
}

Multinominal <- function(N, zeta = 1.5) {
  Z1 <- rnorm(N)

  Z2 <- rnorm(N)

  xb1 <- Z2 + zeta*Z1*Z2 + zeta*Z1

  xb2 <- Z2 - zeta*Z1

  xp1 <- 1/(1+exp(xb1) + exp(xb2))
  xp2 <- exp(xb1) /(1+exp(xb1) + exp(xb2))
  random <- runif(N,0, 1)
  X <- ifelse(random < xp1, "C", ifelse(random < xp1 + xp2,"A","B"))

  yb1 = zeta*Z1*Z2
  yb2 <- exp(Z2) +  zeta*Z1

  yp1 <- 1/(1+exp(yb1) + exp(yb2))
  yp2 <- exp(yb1) /(1+exp(yb1) + exp(yb2))
  random <- runif(N,0, 1)
  Y <- ifelse(random < yp1, "X", ifelse(random < yp1 + yp2,"Y","Z"))

  df <- data.frame(Z1,Z2,X,Y)

  return(df)
}

BinaryData <- function(N, threshold = 0) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)

  threshold <- threshold

  X <- ifelse(rnorm(N, Z1 + Z2 + Z1*Z2, 1) < threshold, 1, 0)

  Y <- ifelse(rnorm(N, Z1 + Z2 + Z1*Z2, 1) < threshold, 1, 0)

  df <- data.frame(Z1,Z2,X,Y)

  return(df)
}

########## Continuous Multivariate Functions ###########

NormalData <- function(N){
  Z1 <- rnorm(N,0,1)
  Z2 <- rnorm(N,0,1)
  X <- rnorm(N, Z1 + Z2, 1)
  Y <- rnorm(N, Z1 + Z2, 1)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}

NonLinNormal <- function(N){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = exp(Z1*Z2) + rnorm(N,0,1)
  Y <- Z1*Z2 + rnorm(N,0,1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

UniformNoise <- function(N) {
  Z1 = rnorm(N, 0, 1)
  Z2 = rnorm(N, 0, 1)
  X = Z2 + Z1 + Z2 * Z1 + runif(N, min=-2, max=2)
  Y = Z2 + Z1 + Z2 * Z1 + runif(N, min=-2, max=2)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}

ExponentialNoise <- function(N, rate_param = 1) {
  Z1 = rnorm(N, 0, 1)
  Z2 = rnorm(N, 0, 1)
  rate_param = rate_param
  X = Z2 + Z1 + Z2 * Z1 + rexp(N, rate = rate_param) - (1 / rate_param)
  Y = Z2 + Z1 + Z2 * Z1 + rexp(N, rate = rate_param) - (1 / rate_param)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}

PoissonNoise <- function(N){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = Z2*Z1 + (rpois(N, lambda = 1)-1)
  Y = Z2*Z1  + (rpois(N, lambda = 1)-1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

SinusoidalData <- function(N, a = 1){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  Z <- Z1 + Z2
  X = exp(-(Z)^2 / 2) * sin(a * (2*Z1 + 0.1*Z2)) + rnorm(N,0,0.1)
  Y = exp(-(Z)^2 / 2) * sin(a * (2*Z2 + 0.1*Z1)) + rnorm(N,0,0.1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

NonLinNormal10 <- function(N) {
  Z <- replicate(10, rnorm(N, 0, 1))
  colnames(Z) <- paste0("Z", 1:10)
  Z_df <- as.data.frame(Z)

  X <- Z[,1] * Z[,2] + sin(Z[,3] * Z[,4]) + abs(Z[,5]) + rnorm(N, 0, 1)
  Y <- Z[,1] * Z[,2] + cos(Z[,6] * Z[,7]) - abs(Z[,8]) + rnorm(N, 0, 1)

  df <- cbind(Z_df, X = X, Y = Y)
  return(df)
}
