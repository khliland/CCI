% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_tuned_params}
\alias{get_tuned_params}
\title{Get the best parameters after tuning with CCI.tuner}
\usage{
get_tuned_params(tuned_model)
}
\arguments{
\item{tuned_model}{A model object returned from the CCI.pretuner function. This object contains the tuned parameters and other relevant information.}
}
\value{
A named list of tuned parameters specific to the model method (e.g., \code{mtry} for random forest, \code{eta}, \code{max_depth} for xgboost). Returns \code{NULL} for unsupported methods.
}
\description{
Get the best parameters after tuning with CCI.tuner
}
\examples{
set.seed(123)
data_generator <-  function(N){
Z1 <- rnorm(N,0,1)
Z2 <- rnorm(N,0,1)
X <- rnorm(N, Z1 + Z2, 1)
Y <- rnorm(N, Z1 + Z2, 1)
df <- data.frame(Z1, Z2, X, Y)
return(df)
}
dat <- data_generator(250)
tuned_model <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
data = dat,
method = 'xgboost',
samples = 3)
tuned_params <- get_tuned_params(tuned_model$best_param)
print(tuned_params)

}
