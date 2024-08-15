
CategorizeInteractiondData <- function(N) {
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

simulateExpLogData <- function(N) {
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

simulateTrigData <- function(N) {
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

simulatePolyData <- function(N) {
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

simulateNonLinearData <- function(N) {
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

simulateComplexCategorization <- function(N) {
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

multinominal_data <- function(N, zeta = 1.5) {
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

binomial_data <- function(n, coef_Z1, coef_Z2, intercept = 0, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  Z1 <- rnorm(n)
  Z2 <- rnorm(n)

  X <- rnorm(n, Z1 + Z2, 1)
  log_odds <- intercept + coef_Z1 * Z1 + coef_Z2 * Z2
  prob <- 1 / (1 + exp(-log_odds))
  Y <- rbinom(n, size = 1, prob = prob)

    data <- data.frame(Y = Y, X = X, Z1 = Z1, Z2 = Z2)

  return(data)
}

categorical_data <- function(N) {

  Z1 <- rnorm(N, mean = 0, sd = 1)
  Z2 <- rnorm(N, mean = 0, sd = 1)
  X <- rnorm(N, Z1 + Z2, sd = 1)

  logits <- cbind(1  - 0.5 * Z1,
                  -1  + 0.5 * Z2,
                  1 - 0.5 * Z1 + 0.5 * Z2)

  exp_logits <- exp(logits)
  probs <- exp_logits / rowSums(exp_logits)
  Y <- apply(probs, 1, function(p) sample(1:3, 1, prob = p)) - 1
  data <- data.frame(Y, X, Z1, Z2)

  return(data)
}

########## Continuous Multivariate Functions ###########

normal_data <- function(N){
  Z1 <- rnorm(N,0,1)
  Z2 <- rnorm(N,0,1)
  X <- rnorm(N, Z1 + Z2, 1)
  Y <- rnorm(N, Z1 + Z2, 1)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}

non_lin_normal <- function(N){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = exp(Z1*Z2) + rnorm(N,0,1)
  Y <- Z1*Z2 + rnorm(N,0,1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

uniform_noise <- function(N) {
  Z1 = rnorm(N, 0, 1)
  Z2 = rnorm(N, 0, 1)
  X = Z2 + Z1 + Z2 * Z1 + runif(N, min=-2, max=2)
  Y = Z2 + Z1 + Z2 * Z1 + runif(N, min=-2, max=2)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}

exponential_noise <- function(N, rate_param = 1) {
  Z1 = rnorm(N, 0, 1)
  Z2 = rnorm(N, 0, 1)
  rate_param = rate_param
  X = Z2 + Z1 + Z2 * Z1 + rexp(N, rate = rate_param) - (1 / rate_param)
  Y = Z2 + Z1 + Z2 * Z1 + rexp(N, rate = rate_param) - (1 / rate_param)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}

poisson_noise <- function(N){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = Z2*Z1 + (rpois(N, lambda = 1)-1)
  Y = Z2*Z1  + (rpois(N, lambda = 1)-1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

sinusoidal <- function(N, a = 1){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  Z <- Z1 + Z2
  X = exp(-(Z)^2 / 2) * sin(a * (2*Z1 + 0.1*Z2)) + rnorm(N,0,0.1)
  Y = exp(-(Z)^2 / 2) * sin(a * (2*Z2 + 0.1*Z1)) + rnorm(N,0,0.1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

