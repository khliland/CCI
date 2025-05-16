#' Generate Normal Data for Conditional Independence Testing
#'
#' This function generates continuous data where X and Y are both functions of Z1 and Z2 with added normal noise.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
NormalData <- function(N){
  Z1 <- rnorm(N,0,1)
  Z2 <- rnorm(N,0,1)
  X <- rnorm(N, Z1 + Z2, 1)
  Y <- rnorm(N, Z1 + Z2, 1)

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
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
sineGaussian_biv <- function(N, a = 1){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = (exp(-(Z1)^2 / 2) * sin(a * (Z1))) - (exp(-(Z2)^2 / 2) * sin(a * (Z2))) + 0.3*rnorm(N,0,0.1)
  Y = (exp(-(Z1)^2 / 2) * sin(a * (Z1))) + (exp(-(Z2)^2 / 2) * sin(a * (Z2))) + 0.3*rnorm(N,0,0.1)

  return(data.frame(Z1,Z2,X,Y))
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
#'
sineGaussian_noise <- function(N, a = 1, d = 0){
  Z = rnorm(N,0,1)
  X = exp(-(Z)^2 / 2) * sin(a * (Z))*rnorm(N,0,1)
  Y = exp(-(Z)^2 / 2) * sin(a * (Z))*rnorm(N,0,1) + d*X

  return(data.frame(Z,X,Y))
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
#'
NonLinearCategorization <- function(N, d = 0) {
  Z <- runif(N, -1, 1)
  X <- rnorm(N, mean = Z, sd = 1)
  Y <- character(N)

  for (i in 1:N) {
    score_y <- cos(Z[i] * pi) + Z[i] + d * X[i]

    if (score_y > 1) {
      Y[i] <- "Very High"
    } else if (score_y > 0.5) {
      Y[i] <- "High"
    } else if (score_y > 0) {
      Y[i] <- "Medium"
    } else {
      Y[i] <- "Low"
    }
  }

  return(data.frame(Z, X, Y = factor(Y, levels = c("Low", "Medium", "High", "Very High"))
  ))
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
      X[i] <- "Category F"
    } else if (score_x > 0.5) {
      X[i] <- "Category A"
    } else if (score_x > 0) {
      X[i] <- "Category X"
    } else {
      X[i] <- "Category 99"
    }
  }

  for (i in 1:N) {
    score_y <- cos(Z1[i] * pi) + Z2[i]

    if (score_y > 1) {
      Y[i] <- "Category A"
    } else if (score_y > 0.5) {
      Y[i] <- "Category 99"
    } else if (score_y > 0) {
      Y[i] <- "Category F"
    } else {
      Y[i] <- "Category X"
    }
  }


  return(data.frame(Z1, Z2, X, Y))
}

#' Generate Bivariate Multinomial Categorical Data
#'
#' Creates a multinomial dataset where the probabilities are nonlinear functions of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#' @param zeta Numeric. Strength of interaction. Default is 1.5.
#'
#' @return A data frame with columns Z1, Z2, X, and Y (both factors).
#' @export
#'
BivMultinominal <- function(N, zeta = 1.5) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)

  xb1 <- Z2 + zeta*Z1*Z2 + Z1
  xb2 <- Z2 - Z1

  xp1 <- 1/(1+exp(xb1) + exp(xb2))
  xp2 <- exp(xb1) /(1+exp(xb1) + exp(xb2))

  random <- runif(N,0, 1)

  X <- ifelse(random < xp1, 0, ifelse(random < xp1 + xp2,1,2))

  yb1 = zeta*Z1*Z2
  yb2 <- exp(Z2) +  Z1 + d*X

  yp1 <- 1/(1+exp(yb1) + exp(yb2))
  yp2 <- exp(yb1) /(1+exp(yb1) + exp(yb2))

  random <- runif(N,0, 1)

  Y <- ifelse(random < yp1, 0, ifelse(random < yp1 + yp2,1,2))

  return( data.frame(Z1,Z2, X,Y))
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
  X <- character(N)
  Y <- character(N)

  for (i in 1:N) {
    if (Z1[i] < 0 && Z2[i] < 0) {
      X[i] <- "V"
    } else if (Z1[i] < 0 && Z2[i] >= 0) {
      X[i] <- "W"
    } else if (Z1[i] >= 0 && Z2[i] < 0) {
      X[i] <- "G"
    } else {
      X[i] <- "U"
    }

    if (Z1[i] + Z2[i] < -1) {
      Y[i] <- "W"
    } else if (Z1[i] + Z2[i] < 0) {
      Y[i] <- "V"
    } else if (Z1[i] + Z2[i] < 1) {
      Y[i] <- "G"
    } else {
      Y[i] <- "U"
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
  X <- character(N)
  Y <- character(N)

  for (i in 1:N) {
    X[i] <- ifelse(exp(Z1[i]) + Z2[i] > 1.5, "Category A",
                   ifelse(exp(Z1[i]) + Z2[i] > 0.5, "Category C",
                          ifelse(exp(Z1[i]) > 0, "Category S", "Category B")))
    Y[i] <- ifelse(log(abs(Z1[i]) + 1) + Z2[i] > 0.5, "Category C",
                   ifelse(log(abs(Z1[i]) + 1) + Z2[i] > 0, "Category A",
                          ifelse(log(abs(Z1[i]) + 1) > -0.5, "Category S", "Category B")))
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
  X <- character(N)
  Y <- character(N)

  for (i in 1:N) {
    X[i] <- ifelse(sin(Z1[i]) + cos(Z2[i]) > 1, "Category A",
                   ifelse(sin(Z1[i]) + cos(Z2[i]) > 0, "Category B",
                          ifelse(sin(Z1[i]) > -1, "Category D", "Category C")))

    Y[i] <- ifelse(cos(Z1[i]) - sin(Z2[i]) > 1, "Blue",
                   ifelse(cos(Z1[i]) - sin(Z2[i]) > 0, "Green",
                          ifelse(cos(Z1[i]) > -1, "Yellow", "Red")))
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
  X <- character(N)
  Y <- character(N)

  for (i in 1:N) {
    X[i] <- ifelse(Z1[i]^2 + Z2[i]^2 > 2, "Down",
                   ifelse(Z1[i]^2 + Z2[i] > 0.5, "Up",
                          ifelse(Z1[i] + Z2[i]^2 > 0, "Left", "Right")))

    Y[i] <- ifelse(Z1[i]^3 + Z2[i] > 1, "East",
                   ifelse(Z1[i]^2 - Z2[i]^2 > 0, "West",
                          ifelse(Z1[i] - Z2[i]^3 > -1, "North", "South")))
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
#'
NonLinearData <- function(N) {
  Z1 <- runif(N, -1, 1)
  Z2 <- runif(N, -1, 1)
  X <- character(N)
  Y <- character(N)


  for (i in 1:N) {

    if (sin(Z1[i] * pi) + Z2[i] > 1) {
      X[i] <- "Very High"
    } else if (sin(Z1[i] * pi) + Z2[i] > 0.5) {
      X[i] <- "High"
    } else if (sin(Z1[i] * pi) + Z2[i] > 0) {
      X[i] <- "Medium"
    } else {
      X[i] <- "Low"
    }


    if (cos(Z1[i] * pi) + Z2[i] > 1) {
      Y[i] <- "Class A"
    } else if (cos(Z1[i] * pi) + Z2[i] > 0.5) {
      Y[i] <- "Class B"
    } else if (cos(Z1[i] * pi) + Z2[i] > 0) {
      Y[i] <- "Class C"
    } else {
      Y[i] <- "Class D"
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
#'
#' @examples
#' head(ComplexCategorization(100))
#'
ComplexCategorization <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)
  X <- character(N)
  Y <- character(N)

  for (i in 1:N) {
    # Define X categories based on the quadrant
    X[i] <- if (Z1[i] > 0 && Z2[i] > 0) {
      "Northeast"
    } else if (Z1[i] > 0 && Z2[i] <= 0) {
      "Southeast"
    } else if (Z1[i] <= 0 && Z2[i] > 0) {
      "Northwest"
    } else {
      "Southwest"
    }

    Y_score <- Z1[i] + Z2[i]

    Y[i] <- if (Y_score > 1) {
      "High Risk"
    } else if (Y_score > 0) {
      "Moderate Risk"
    } else if (Y_score > -1) {
      "Low Risk"
    } else {
      "Minimal Risk"
    }
  }
 return(data.frame(Z1, Z2, X, Y))
}

#' Generate Binary Data
#'
#' Creates binary data based on a nonlinear interaction of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#' @param threshold Numeric. Threshold for binary classification. Default is 0.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
#' @examples
#' head(BinaryData(100))
#'
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
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
#' @examples
#' head(NonLinNormal(N = 100))
#'
NonLinNormal <- function(N, d = 0){
  Z1 <- rnorm(N,0,1)
  Z2 <- rnorm(N,0,1)
  X <- Z1*Z2 + rnorm(N,0,1)
  Y <- exp(Z1*Z2) + rnorm(N,0,1)

  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

#' Generate Data with Uniform Noise
#'
#' Adds uniform noise to a nonlinear combination of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
#' @examples
#' head(UniformNoise(100))
#'
UniformNoise <- function(N) {
  Z1 = rnorm(N, 0, 1)
  Z2 = rnorm(N, 0, 1)
  X = Z2 - Z1 - Z2 * Z1 + runif(N, min=-2, max=2)
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
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
#' @examples
#' head(ExponentialNoise(100))
ExponentialNoise <- function(N, rate_param = 1) {
  Z1 = rnorm(N, 0, 1)
  Z2 = rnorm(N, 0, 1)
  rate_param = rate_param
  X = Z2 - Z1 - Z2 * Z1 + rexp(N, rate = rate_param) - (1 / rate_param)
  Y = Z2 + Z1 + Z2 * Z1 + rexp(N, rate = rate_param) - (1 / rate_param)


  df <- data.frame(Z1, Z2, X, Y)
  return(df)
}
#' Generate Data with Poisson Noise
#'
#' Adds Poisson noise to a nonlinear combination of Z1 and Z2.
#'
#' @param N Integer. Sample size.
#' @param lambda Numeric. Rate parameter for the Poisson distribution. Default is 1.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
#' @examples
#' head(PoissonNoise(100))
#'
PoissonNoise <- function(N, lambda = 1){
  Z1 = rnorm(N,0,1)
  Z2 = rnorm(N,0,1)
  X = Z2*Z1 + (rpois(N, lambda = lambda)-1)
  Y = Z2*Z1  + (rpois(N, lambda = lambda)-1)

  df <- data.frame(Z1,Z2,X,Y)
  return(df)
}

#' Generate High-dimensional Nonlinear Normal Data
#'
#' Creates a Z-dimensional nonlinear dataset with complex dependencies between features and targets.
#'
#' @param N Integer. Sample size.
#' @param d Numeric. Dependency strength. Default is 0.
#' @param Zs Integer. Number of Z variables. Default is 10.
#'
#' @return A data frame with columns Z1-Z10, X, and Y.
#' @export
#'
#' @examples
#' head(NonLinNormalZs(N = 100, Zs  = 20))
#'
NonLinNormalZs <- function(N, d = 0, Zs = 20) {
  Z <- replicate(Zs, rnorm(N, 0, 1))
  colnames(Z) <- paste0("Z", 1:Zs)
  Z_df <- as.data.frame(Z)

  X <- Z[,1] * Z[,2] + sin(Z[,3] * Z[,4]) + abs(Z[,5]) + rnorm(N, 0, 1)
  Y <- Z[,1] * Z[,2] + cos(Z[,6] * Z[,7]) - abs(Z[,8]) + rnorm(N, 0, 1) + d*X

  df <- cbind(Z_df, X = X, Y = Y)
  return(df)
}
#' Generate Quadratic Threshold Data
#'
#' Generates data with a quadratic threshold effect based on Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
#' @examples
#' head(quadThreshContXSim(100))

quadThreshContXSim <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)

  X <- Z1 + 2 * Z2 + rnorm(N, 0, 0.2)  # continuous linear combination
  Y <- ifelse(Z1 + Z2 > 1, "Strong",
              ifelse(Z1 + Z2 > 0, "Weak",
                     ifelse(Z1 + Z2 > -1, "Medium", 0)))


  return(data.frame(Z1, Z2, X, Y))
}

#' Generate Grid Partitioned Data
#'
#' Generates data with a grid partitioning effect based on Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
#' @examples
#' head(gridPartitionContXSim(100))
#'
gridPartitionContXSim <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)

  X <- sin(pi * Z1) + cos(pi * Z2) + rnorm(N, 0, 0.2)  # continuous nonlinear combo
  Y <- ifelse(Z1 + Z2 < -1, "High",
              ifelse(Z1 + Z2 < 0, "Low",
                     ifelse(Z1 + Z2 < 1, "Medium", "No opinion")))

  return(data.frame(Z1, Z2, X, Y))
}

#' Generate Polynomial Decision Boundary Data
#'
#' Generates data with a polynomial decision boundary based on Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
#' @examples
#' head(polyDecisionContXSim(100))
#'
polyDecisionContXSim <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)

  X <- Z1^2 + Z2^2 + rnorm(N, 0, 1)
  Y <- ifelse(Z1^3 + Z2 > 1, "Blue",
              ifelse(Z1^2 - Z2^2 > 0, "White",
                     ifelse(Z1 - Z2^3 > -1, "Black", "Red")))


  return(data.frame(Z1, Z2, X, Y))
}

#' Generate Sinusoidal and Cosine Data
#'
#' Generates data with sinusoidal and cosine dependencies based on Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#' @export
#'
#' @examples
#' head(sinCosThresholdContXSim(100))

sinCosThresholdContXSim <- function(N) {
  Z1 <- runif(N, -1, 1)
  Z2 <- runif(N, -1, 1)

  X <- sin(Z1 * pi) + Z2 + rnorm(N, 0, 0.1)

  Y <- ifelse(cos(Z1 * pi) + Z2 > 1, "Laptop",
              ifelse(cos(Z1 * pi) + Z2 > 0.5, "Desktop",
                     ifelse(cos(Z1 * pi) + Z2 > 0, "GamePad", "Phone")))


  return(data.frame(Z1, Z2, X, Y))
}

#' Generate Exponential and Logarithmic Data
#'
#' Generates data with exponential and logarithmic dependencies based on Z1 and Z2.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns Z1, Z2, X, and Y.
#'
#' @export
#'
#' @examples
#' head(expLogThresholdContXSim(100))

expLogThresholdContXSim <- function(N) {
  Z1 <- rnorm(N)
  Z2 <- rnorm(N)

  X <- exp(Z1) + Z2 + rnorm(N, 0, 0.2)
  Y <- ifelse(log(abs(Z1) + 1) + Z2 > 0.5, "Goblin",
              ifelse(log(abs(Z1) + 1) + Z2 > 0, "Orc",
                     ifelse(log(abs(Z1) + 1) > -0.5, "Troll", "Elf")))


  return(data.frame(Z1, Z2, X, Y))
}

#' Generate Hard Case Data with Two Z Variables
#'
#' Generates data with a hard case scenario where X and Y are influenced by two Z variables in a nonlinear manner.
#'
#' @param N Integer. Sample size.
#'
#' @return A data frame with columns X, Y, Z1, and Z2.
#' @export
#'
#' @examples
#' head(hard_case_twoZ_sim(100))
#'
hard_case <- function(N) {
  Z1 <- runif(N, -2, 2)
  Z2 <- runif(N, -2, 2)

  hZ <- sin(Z1) * cos(Z2)  # Shared nonlinear component

  # X and Y both depend on the same nonlinear function of Z1, Z2, but are otherwise independent
  X <- hZ + 0.2 * rnorm(N)
  Y <- hZ^2 + 0.2 * rnorm(N)

  data.frame(X, Y, Z1, Z2)
}
