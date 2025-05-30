% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{NonLinNormalZs}
\alias{NonLinNormalZs}
\title{Generate High-dimensional Nonlinear Normal Data}
\usage{
NonLinNormalZs(N, d = 0, Zs = 20)
}
\arguments{
\item{N}{Integer. Sample size.}

\item{d}{Numeric. Dependency strength. Default is 0.}

\item{Zs}{Integer. Number of Z variables. Default is 10.}
}
\value{
A data frame with columns Z1-Z10, X, and Y.
}
\description{
Creates a Z-dimensional nonlinear dataset with complex dependencies between features and targets.
}
\examples{
head(NonLinNormalZs(N = 100, Zs  = 20))

}
