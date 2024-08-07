% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrappers.R
\name{glm_wrapper}
\alias{glm_wrapper}
\title{Wrapper function for GLM model training and evaluation}
\usage{
glm_wrapper(
  formula,
  data,
  train_indices,
  test_indices,
  lm_family,
  data_type,
  ...
)
}
\arguments{
\item{formula}{Model formula}

\item{data}{Data frame}

\item{train_indices}{Indices for training data}

\item{test_indices}{Indices for testing data}

\item{lm_family}{Family for GLM}

\item{data_type}{Type of data (continuous or binary)}

\item{...}{Additional arguments passed to glm}

\item{iteration}{Current iteration index}
}
\value{
Performance metric (RMSE for continuous, Kappa for binary)
}
\description{
Wrapper function for GLM model training and evaluation
}
