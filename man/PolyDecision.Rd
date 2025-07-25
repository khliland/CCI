% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{PolyDecision}
\alias{PolyDecision}
\title{Generate Polynomial Decision Boundary Data}
\usage{
PolyDecision(N)
}
\arguments{
\item{N}{Integer. Sample size.}
}
\value{
A data frame with columns Z1, Z2, X, and Y.
}
\description{
Generates data with a polynomial decision boundary based on Z1 and Z2.
}
\examples{
head(PolyDecision(100))

}
