#' Generate Normal Data for Conditional Independence Testing
#'
#' This function generates continuous data where X and Y are both functions of Z1 and Z2 with added normal noise.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
NormalData <- function(N, d = 0){
  Z1 <- rnorm(N,0,1)
  Z2 <- rnorm(N,0,1)
  X <- rnorm(N, Z1 + Z2, 1)
  Y <- rnorm(N, Z1 + Z2 + d*X, 1)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}
#' Generate Sine-Gaussian Data (Univariate)
#'
#' This function generates data with a nonlinear sinusoidal dependency based on a Gaussian density envelope.
#'
#' @param N Integer. Sample size.
#' @param a Numeric. Frequency parameter of the sine function. Default is 1.
#' @param d Numeric. Strength of dependency between X and Y. Default is 0.
#'
#' @return A data frame with columns Z, X, and Y.
#' @export
sineGaussian <- function(N, a = 1, d = 0){
  Z = rnorm(N,0,1)
  X = exp(-(Z)^2 / 2) * sin(a * (Z)) + 0.3*rnorm(N,0,0.1)
  Y = exp(-(Z)^2 / 2) * sin(a * (Z)) +  d*X + 0.3*rnorm(N,0,0.1)
  df <- data.frame(Z,X,Y)
  return(df)
}
#' Generate Sine-Gaussian Data (Bivariate)
#'
#' This function generates bivariate data with nonlinear dependencies based on a Gaussian density envelope and sinusoidal functions.
#'
#' @param N Integer. Sample size.
#' @param a Numeric. Frequency parameter for the sine function. Default is 1.
#' @param d Numeric. Strength of dependency between X and Y. Default is 0.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
sineGaussian_biv <- function(N, a = 1, d = 0){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = (exp(-(Z1)^2 / 2) * sin(a * (Z1))) - (exp(-(Z2)^2 / 2) * sin(a * (Z2))) + 0.3*rnorm(N,0,0.1)
  Y = (exp(-(Z1)^2 / 2) * sin(a * (Z1))) + (exp(-(Z2)^2 / 2) * sin(a * (Z2))) + d*X + 0.3*rnorm(N,0,0.1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

#' Generate Sine-Gaussian Data (Bivariate)
#'
#' This function generates bivariate data with nonlinear dependencies based on a Gaussian density envelope and sinusoidal functions.
#'
#' @param N Integer. Sample size.
#' @param a Numeric. Frequency parameter for the sine function. Default is 1.
#' @param d Numeric. Strength of dependency between X and Y. Default is 0.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
sineGaussian_noise <- function(N, a = 1, d = 0){
  Z = rnorm(N,0,1)
  X = exp(-(Z)^2 / 2) * sin(a * (Z))*rnorm(N,0,1)
  Y = exp(-(Z)^2 / 2) * sin(a * (Z))*rnorm(N,0,1) + d*X

  df <- data.frame(Z,X,Y)
  return(df)
}

#' Generate Nonlinear Categorical Data (Univariate)
#'
#' Generates a dataset with a single Z influencing categorical X and Y.
#'
#' @param N Integer. Sample size.
#' @param d Numeric. Dependency strength. Default is 0.
#'
#' @return A data frame with columns Z, X, and Y.
#' @export
NonLinearCategorization <- function(N, d = 0) {
  Z <- runif(N, -1, 1)
  X <- rnorm(Z,0,1)
  Y <- numeric(N)


  for (i in 1:N) {
    score_y <- cos(Z[i] * pi) + Z[i] + d * X[i] #Breaking independence by setting d not equal 0

    if (score_y > 1) {
      Y[i] <- 3
    } else if (score_y > 0.5) {
      Y[i] <- 2
    } else if (score_y > 0) {
      Y[i] <- 1
    } else {
      Y[i] <- 0
    }
  }
  return(data.frame(Z, X = X, Y = Y))
}
#' Generate Bivariate Nonlinear Categorical Data
#'
#' Generates categorical variables X and Y based on nonlinear combinations of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
BivNonLinearCategorization <- function(N) {
  Z1 <- runif(N, -2, 2)
  Z2 <- runif(N, -2,2)
  X <- character(N)
  Y <- character(N)

  for (i in 1:N) {
    score_x <- sin(Z2[i] * pi) + Z1[i]
    if (score_x > 1) {
      X[i] <- "Category 1"
    } else if (score_x > 0.5) {
      X[i] <- "Category 2"
    } else if (score_x > 0) {
      X[i] <- "Category 4"
    } else {
      X[i] <- "Category 3"
    }
  }

  for (i in 1:N) {
    score_y <- cos(Z1[i] * pi) + Z2[i]

    if (score_y > 1) {
      Y[i] <- "Category 3"
    } else if (score_y > 0.5) {
      Y[i] <- "Category 1"
    } else if (score_y > 0) {
      Y[i] <- "Category 2"
    } else {
      Y[i] <- "Category 4"
    }
  }
  return(data.frame(Z1 = Z1, Z2 = Z2, X = as.factor(X), Y = as.factor(Y)))
}

#' Generate Bivariate Multinomial Categorical Data
#'
#' Creates a multinomial dataset where the probabilities are nonlinear functions of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#' @param zeta Numeric. Strength of interaction. Default is 1.5.
#' @param d Numeric. Dependency strength between X and Y. Default is 0.
#'
#' @return A data frame with columns Z1, Z2, X, and Y (both factors).
#' @export
BivMultinominal <- function(N, zeta = 1.5, d = 0) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)

  xb1 <- Z2 + zeta*Z1*Z2 + zeta*Z1
  xb2 <- Z2 - zeta*Z1

  xp1 <- 1/(1+exp(xb1) + exp(xb2))
  xp2 <- exp(xb1) /(1+exp(xb1) + exp(xb2))

  random <- runif(N,0, 1)

  X <- ifelse(random < xp1, 0, ifelse(random < xp1 + xp2,1,2))

  yb1 = zeta*Z1*Z2
  yb2 <- exp(Z2) +  zeta*Z1

  yp1 <- 1/(1+exp(yb1) + exp(yb2))
  yp2 <- exp(yb1) /(1+exp(yb1) + exp(yb2))

  random <- runif(N,0, 1)

  Y <- ifelse(random < yp1, 0, ifelse(random < yp1 + yp2,1,2))

  df <- data.frame(Z1 = Z1,Z2 = Z2, X = as.factor(X),Y = as.factor(Y))

  return(df)
}
#' Generate Categorical Data Based on Interactions
#'
#' Creates categorical X and Y variables based on the interaction of signs and sums of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
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
#' Generate Categorical Data Based on Exponential and Logarithmic Functions
#'
#' Categorizes based on thresholds of exponential and logarithmic transformations of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
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
#' Generate Categorical Trigonometric Data
#'
#' Uses sine and cosine functions of Z1 and Z2 to generate categorical outcomes.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
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
#' Generate Categorical Polynomial Data
#'
#' Generates X and Y categories based on polynomial combinations of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
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

#' Generate Nonlinear Categorical Data (Bivariate)
#'
#' Creates categorical X and Y variables based on sinusoidal and cosine functions of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
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

#' Generate Complex Categorical Data
#'
#' A more intricate categorization based on combinations of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
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

#' Generate Multinomial Categorical Data
#'
#' Multinomial categorical variables X and Y generated via nonlinear logits of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#' @param zeta Numeric. Strength of interaction. Default is 1.5.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export

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


#' Generate Nonlinear Normal Data
#'
#' Creates nonlinear continuous data based on an exponential interaction of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
NonLinNormal <- function(N){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = exp(Z1*Z2) + rnorm(N,0,1)
  Y <- Z1*Z2 + rnorm(N,0,1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

#' Generate Data with Uniform Noise
#'
#' Adds uniform noise to a nonlinear combination of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
UniformNoise <- function(N) {
  Z1 = rnorm(N, 0, 1)
  Z2 = rnorm(N, 0, 1)
  X = Z2 + Z1 + Z2 * Z1 + runif(N, min=-2, max=2)
  Y = Z2 + Z1 + Z2 * Z1 + runif(N, min=-2, max=2)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}

#' Generate Data with Exponential Noise
#'
#' Adds exponential noise to a nonlinear combination of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#' @param rate_param Numeric. Rate parameter for the exponential distribution. Default is 1.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
ExponentialNoise <- function(N, rate_param = 1) {
  Z1 = rnorm(N, 0, 1)
  Z2 = rnorm(N, 0, 1)
  rate_param = rate_param
  X = Z2 + Z1 + Z2 * Z1 + rexp(N, rate = rate_param) - (1 / rate_param)
  Y = Z2 + Z1 + Z2 * Z1 + rexp(N, rate = rate_param) - (1 / rate_param)
  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}
#' Generate Data with Poisson Noise
#'
#' Adds Poisson noise to a nonlinear combination of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
PoissonNoise <- function(N){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = Z2*Z1 + (rpois(N, lambda = 1)-1)
  Y = Z2*Z1  + (rpois(N, lambda = 1)-1)
  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

#' Generate High-dimensional Nonlinear Normal Data
#'
#' Creates a 10-dimensional nonlinear dataset with complex dependencies between features and targets.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1-Z10, X, and Y.
#' @export
NonLinNormal10 <- function(N) {
  Z <- replicate(10, rnorm(N, 0, 1))
  colnames(Z) <- paste0("Z", 1:10)
  Z_df <- as.data.frame(Z)

  X <- Z[,1] * Z[,2] + sin(Z[,3] * Z[,4]) + abs(Z[,5]) + rnorm(N, 0, 1)
  Y <- Z[,1] * Z[,2] + cos(Z[,6] * Z[,7]) - abs(Z[,8]) + rnorm(N, 0, 1)

  df <- cbind(Z_df, X = X, Y = Y)
  return(df)
}
