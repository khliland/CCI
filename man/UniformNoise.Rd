% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{UniformNoise}
\alias{UniformNoise}
\title{Generate Data with Uniform Noise}
\usage{
UniformNoise(N)
}
\arguments{
\item{N}{Integer. Sample size.}
}
\value{
A data frame with columns Z1, Z2, X, and Y.
}
\description{
Adds uniform noise to a nonlinear combination of Z1 and Z2.
}
\examples{
head(UniformNoise(100))

}
