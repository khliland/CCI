% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrappers.R
\name{wrapper_ranger}
\alias{wrapper_ranger}
\title{Random Forest wrapper for CCI}
\usage{
wrapper_ranger(
  formula,
  data,
  train_indices,
  test_indices,
  metric,
  metricfunc = NULL,
  nthread = 1,
  ...
)
}
\arguments{
\item{formula}{Model formula specifying the dependent and independent variables.}

\item{data}{Data frame containing the dataset to be used for training and testing the model.}

\item{train_indices}{A vector of indices specifying the rows in \code{data} to be used as the training set.}

\item{test_indices}{A vector of indices specifying the rows in \code{data} to be used as the test set.}

\item{metric}{Character string indicating the type of performance metric. Can be "RMSE" for regression, "Kappa" for binary classification, or multiclass classification.}

\item{metricfunc}{Optional user-defined function to calculate a custom performance metric. This function should take the arguments \code{data}, \code{model}, and \code{test_indices}, and return a numeric value representing the performance metric.}

\item{nthread}{Integer. The number of threads to use for parallel processing. Default is 1.}

\item{...}{Additional arguments passed to the \code{ranger} function.}
}
\value{
A numeric value representing the performance metric of the model on the test set.
}
\description{
Random Forest wrapper for CCI
}
